#include "MAPL_Generic.h"

module mapl3g_Cap
   use mapl3g_CapGridComp, only: cap_setservices => setServices
   use mapl3g_GenericGridComp, only: generic_setservices => setServices
   use esmf
   implicit none
   private

   public :: run

contains

   !    model              |       pfio         |        mit
   !---------------------- |  ----------------- |  -------------
   !                       |                    |
   !    run pfio_client    |  run_server        |  run_server
   !    run mit_client     |                    |
   !    run geos           |                    |
   


   subroutine run(config_filename, unusable, comm, rc)
      character(*), intent(in) :: config_filename
      integer, optional, intent(in) :: comm
      integer, optional, intent(out) :: rc

      type(StringIntegerMap) :: comm_map
      type(ApplicationMode) :: mode ! model or server

      call MAPL_initialize(config_fileName, _RC)

      config = MAPL_HConfigCreate(config_filename, _RC)

      mode = get_mode(config, _RC)
      call mode%run_server(config, _RC)  ! noop for model nodes

      call run_clients(config, _RC) ! noop for server nodes
      call run_model(config, _RC) ! noop for server nodes

      call ESMF_HConfigDestroy(config, nogarbage=.true., _RC)
      call MAPL_Finalize(_RC)
      _RETURN(_SUCCESS)
   end subroutine run


      call comm%run_
      call run_servers

      
      call start_servers(config, _RC)

      has_servers = ESMF_HConfigIsDefined(config, keystring='servers', _RC)
      if (has_servers) then
         ...
         call create_comms(comm, n_nodes_map, comm_map, _RC)

         associate (e => comm_map%end())
           iter = comm_map%begin()
           do while (iter /= e)
              if (iter%second() /= MPI_COMM_NULL) then
                 call something(iter%first(), iter%second())
              end if
           end do
         end associate

         call mpi_finalize(...)

         call ESMF_HConfigSet(config, keystring='servers', value=comm_map, _RC)
      end if


      cap_gridcomp = MAPL_GridCompCreate('CAP', cap_setservices, config, petList=PETS_IN_COMM_GEOS, _RC)
      call ESMF_GridCompSetServices(cap_gridcomp, generic_setServices, userRC=user_status, _RC); _VERIFY(user_status)

      importState = ESMF_StateCreate(_RC)
      exportState = ESMF_StateCreate(_RC)
      clock = create_clock(config, _RC)

      call initialize(cap_gc, importState=importState, exportState=exportState, clock=clock, _RC)

      call ESMF_GridCompRun(cap_gc, & 
           importState=importState, exportState=exportState, clock=clock, &
           userRC=user_status, _RC); _VERIFY(user_status)

      call ESMF_GridCompFinalize(cap_gc, importState=importState, exportState=exportState, clock=clock, &
           userRC=user_status, _RC); _VERIFY(user_status)

      call ESMF_GridCompDestroy(cap_gc, nogarbage=.true., _RC)
      call ESMF_ConfigDestroy(config, nogarbage=.true, _RC)
      call MAPL_Finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine run

  subroutine MAPL_Initialize(config_filename, mpi_communicator, rc)
      character(*), intent(in) :: config_filename
      integer, intent(in) :: mpi_communicator
      integer, optional, intent(out) :: rc

      integer :: status

      ! Cannot process config file until ESMF is initialized, so this is first.
      
      call ESMF_Initialize(configFileName=config_filename, configKey='esmf', &
           mpiCommunicator=mpi_communicator,_RC)
      call profiler_init(...)
      call pflogger_init(...)

      _RETURN(_SUCCESS)
   end subroutine MAPL_Initialize
      
   subroutine MAPL_Finalize(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      ! Cannot process config file until ESMF is initialized, so this is first.

      call profiler_finalize(...)
      call pflogger_finalize(...)
      call ESMF_Finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine MAPL_Finalize
      

   subroutine create_comms(comm, n_nodes_map, comm_map, rc)
      integer, intent(in) :: comm
      type(StringIntegerMap), intent(in) :: n_nodes_map
      type(StringIntegerMap), intent(out) :: comm_map
      integer, optional, intent(out) :: rc
      

      type(StringIntegerMap), intent(out) :: group_map
      integer :: all_grp, new_grp, union_grp, model_grp
      integer :: new_comm
      integer :: n_0, n_1

      call MPI_Comm_group(comm, all_grp, ierror)

      ! 1) Define group for each server (and model)
      associate (e => n_nodes_map%fend())
        iter = n_nodes_map%fbegin()
        n_0 = 0
        do while (iter /= e)
           call iter%next()
           n_1 = n_0 + iter%second() - 1
           call MPI_Group_incl(all_grp, n1-n_0+1, range(n_0, n_1), new_grp, ierror)
           call group_map%insert(iter%first(), new_grp)
           n_0 = n_1 + 1
        end do
      end associate

      ! 2) Construct group that is union of each server with model,
      !    and create a corresponding communicator.
      g_model = group_map%of('model')
      associate (e => n_nodes_map%fend())
        iter = n_nodes_map%fbegin()
        do while (iter /= e)
           call iter%next()
           call MPI_Group_union(g_model, iter%second(), union_group, ierror)
           call MPI_Comm_create_group(comm, union_group, 0, new_comm, ierror)
           call MPI_Group_free(g_union_group, ierror)
           call comm_map%insert(iter%first(), new_comm)
        end do
      end associate

      associate (e => n_nodes_map%fend())
        iter = n_nodes_map%fbegin()
        do while (iter /= e)
           call iter%next()
           call MPI_Group_free(iter%second(), ierror)
        end do
      end associate

   end subroutine create_comms
      
end module mapl3g_Cap
