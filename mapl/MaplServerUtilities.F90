#include "MAPL.h"

module mapl_MaplServerUtilities_mod

   use mapl_ErrorHandling_mod
   use esmf
   use mpi

   implicit none
   private

   public :: ServerResources
   public :: pets_on_ssis
   public :: get_num_ssis
   public :: get_model_petCount
   public :: get_server_hconfigs
   public :: get_ssis_per_server
   public :: create_server_comms
   public :: make_server_gridcomp

   type :: ServerResources
      integer :: world_comm
      integer :: model_comm
      integer :: server_comm
      integer :: nwriter_per_node
      character(:), allocatable :: subclass
   end type ServerResources

contains

   ! Return the PET indices of all PETs whose SSI index falls in [ssi_lo, ssi_hi).
   pure function pets_on_ssis(ssiMap, ssi_lo, ssi_hi) result(pets)
      integer, intent(in) :: ssiMap(:)
      integer, intent(in) :: ssi_lo
      integer, intent(in) :: ssi_hi
      integer, allocatable :: pets(:)

      integer :: n

      pets = pack([(n, n = 0, size(ssiMap)-1)], ssiMap >= ssi_lo .and. ssiMap < ssi_hi)
   end function pets_on_ssis

   ! Count the number of SSIs (nodes) needed to host petCount PETs, starting
   ! from ssiOffset.  Walks ssiMap entries until at least petCount PETs are
   ! accounted for.
   integer function get_num_ssis(petCount, ssiMap, ssiOffset, rc) result(num_ssis)
      integer, intent(in) :: petCount
      integer, intent(in) :: ssiMap(:)
      integer, intent(in) :: ssiOffset
      integer, optional, intent(out) :: rc

      integer :: n
      integer :: found

      num_ssis = 0

      found = 0
      do n = ssiOffset, size(ssiMap) - 1
         found = found + count(ssiMap == n)
         if (found >= petCount) exit
      end do


      _ASSERT(found >= petCount, 'Insufficient resources for running model.')
      num_ssis = 1 + (n - ssiOffset)

      _RETURN(_SUCCESS)
   end function get_num_ssis

   integer function get_model_petCount(vm, hconfig, rc) result(model_petCount)
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_model_petcount

      call ESMF_VMGet(vm, petcount=model_petCount, _RC)

      has_model_petcount = ESMF_HConfigIsDefined(hconfig, keystring='model_petcount', _RC)
      if (has_model_petcount) then
         model_petcount = ESMF_HConfigAsI4(hconfig, keystring='model_petcount', _RC)
      end if

      _RETURN(_SUCCESS)
   end function get_model_petCount

   subroutine get_server_hconfigs(servers_hconfig, server_hconfigs, server_names, rc)
      type(ESMF_HConfig), intent(in) :: servers_hconfig
      type(ESMF_HConfig), allocatable, intent(out) :: server_hconfigs(:)
      character(ESMF_MAXSTR), allocatable, intent(out) :: server_names(:)
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: n_servers, i_server
      type(ESMF_HConfigIter) :: iter_begin, iter_end, iter

      n_servers = ESMF_HConfigGetSize(servers_hconfig, _RC)
      allocate(server_hconfigs(n_servers))
      allocate(server_names(n_servers))

      iter_begin = ESMF_HConfigIterBegin(servers_hconfig,_RC)
      iter_end = ESMF_HConfigIterEnd(servers_hconfig, _RC)
      iter = iter_begin

      i_server = 0
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         i_server = i_server + 1
         ! server_hconfigs(i_server) = ESMF_HConfigCreateAtMapVal(iter, _RC)
         server_hconfigs(i_server) = ESMF_HConfigCreateAt(iter, _RC)
         server_names(i_server) = ESMF_HConfigAsStringMapKey(iter, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine get_server_hconfigs

    ! Resolve num_nodes for each server entry.  The last server may use the
    ! wildcard value '*' to claim all remaining SSIs after model + prior servers.
    ! ssiCount and num_model_ssis are required to resolve the wildcard.
    !
    ! Local servers (local: true) consume zero SSIs and are skipped.
    ! Remote servers must have num_nodes (either integer or '*' wildcard).
    !
    ! Detection strategy: ESMF_HConfigAsString casts integer YAML nodes to string
    ! without error, so reading as string first lets us check for '*' before
    ! attempting an integer read — no spurious ESMF log warnings.
    function get_ssis_per_server(server_hconfigs, ssiCount, num_model_ssis, rc) result(ssis_per_server)
       integer, allocatable :: ssis_per_server(:)
       type(ESMF_HConfig), intent(in) :: server_hconfigs(:)
       integer, intent(in) :: ssiCount
       integer, intent(in) :: num_model_ssis
       integer, optional, intent(out) :: rc

       integer :: status
       integer :: i_server
       integer :: used_ssis
       logical :: is_local, has_local
       logical :: has_num_nodes
       character(:), allocatable :: num_nodes_str

       associate (n_servers => size(server_hconfigs))
          allocate(ssis_per_server(n_servers))
          used_ssis = num_model_ssis
          do i_server = 1, n_servers
             ! Check if this is a local server.
             has_local = ESMF_HConfigIsDefined(server_hconfigs(i_server), keystring='local', _RC)

             is_local = .false. ! unless
             if (has_local) then
                is_local = ESMF_HConfigAsLogical(server_hconfigs(i_server), keystring='local', _RC)
             end if

             if (is_local) then
                ! Local servers consume zero extra SSIs.
                ! Validation: local: true and num_nodes are mutually exclusive.
                has_num_nodes = ESMF_HConfigIsDefined(server_hconfigs(i_server), keystring='num_nodes', _RC)
                _ASSERT(.not. has_num_nodes, "Server entry cannot have both 'local: true' and 'num_nodes'")
                ssis_per_server(i_server) = 0
                cycle
             end if

             ! Remote server: must have num_nodes.
             ! Read as string: ESMF casts integer YAML nodes to string without
             ! complaint, so "2" and "*" both arrive cleanly.
             num_nodes_str = ESMF_HConfigAsString(server_hconfigs(i_server), keystring='num_nodes', _RC)
             if (num_nodes_str == '*') then
                ! Wildcard: takes all remaining SSIs; only valid on the last server.
                _ASSERT(i_server == n_servers, "'*' for num_nodes is only valid for the last server")
                ssis_per_server(i_server) = ssiCount - used_ssis
                _ASSERT(ssis_per_server(i_server) > 0, "PET resources oversubscribed: no SSIs remain for wildcard server")
                cycle
             end if

             ! Integer value — re-read as I4 (ESMF casts without complaint).
             ssis_per_server(i_server) = ESMF_HConfigAsI4(server_hconfigs(i_server), keystring='num_nodes', _RC)
             used_ssis = used_ssis + ssis_per_server(i_server)

          end do
       end associate
       _RETURN(_SUCCESS)
    end function get_ssis_per_server

   ! Create the server and model+server MPI communicators for one server entry.
   ! Allocates and immediately frees the intermediate MPI groups.
   subroutine create_server_comms(world_comm, world_group, model_group, server_pets, server_comm, model_server_comm, rc)
      integer, intent(in) :: world_comm
      integer, intent(in) :: world_group
      integer, intent(in) :: model_group
      integer, intent(in) :: server_pets(:)
      integer, intent(out) :: server_comm
      integer, intent(out) :: model_server_comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: server_group, model_server_group

      call MPI_Group_incl(world_group, size(server_pets), server_pets, server_group, _IERROR)
      call MPI_Group_union(server_group, model_group, model_server_group, _IERROR)
      call MPI_Comm_create_group(world_comm, server_group, 0, server_comm, _IERROR)
      call MPI_Comm_create_group(world_comm, model_server_group, 0, model_server_comm, _IERROR)
      call MPI_Group_Free(server_group, _IERROR)
      call MPI_Group_Free(model_server_group, _IERROR)

      _RETURN(_SUCCESS)
   end subroutine create_server_comms

   function make_server_gridcomp(hconfig, petList, comms, rc) result(gridcomp)
      use mapl_DSO_Utilities_mod
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: petList(:)
      integer, intent(in) :: comms(3) ! world, model, server
      integer, optional, intent(out) :: rc

      integer :: status, user_status
      type(ESMF_HConfig) :: server_hconfig, comms_hconfig
      character(:), allocatable :: sharedObj
      character(:), allocatable :: userRoutine

      server_hconfig = ESMF_HConfigCreateAt(hconfig, _RC)
      comms_hconfig = ESMF_HConfigCreate(content='{}', _RC)
      call ESMF_HConfigAdd(comms_hconfig, comms(1), addKeyString='world_comm', _RC)
      call ESMF_HConfigAdd(comms_hconfig, comms(2), addKeyString='model_comm', _RC)
      call ESMF_HConfigAdd(comms_hconfig, comms(3), addKeyString='server_comm', _RC)
      call ESMF_HConfigAdd(server_hconfig, comms_hconfig, addKeyString='comms', _RC)

      gridcomp = ESMF_GridCompCreate(petList=petList, _RC)
      sharedObj = ESMF_HConfigAsString(server_hconfig, keystring='sharedOb', _RC)
      userRoutine = ESMF_HConfigAsString(server_hconfig, keystring='userRoutine', _RC)
      call ESMF_GridCompSetServices(gridcomp, sharedObj=adjust_dso_name(sharedObj), userRoutine=userRoutine, _USERRC)

      call ESMF_HConfigDestroy(comms_hconfig, _RC)
      call ESMF_HConfigDestroy(server_hconfig, _RC)

      _RETURN(_SUCCESS)
   end function make_server_gridcomp

end module mapl_MaplServerUtilities_mod
