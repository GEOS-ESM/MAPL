#include "MAPL.h"

module mapl_PfioServerGridComp_mod
   use mapl_MaplFramework_mod,       only: MAPL_PublishServer
   use mapl_MaplServerUtilities_mod, only: ServerResources
   use pFIO_MpiServerMod,            only: MpiServer
   use pFIO_MultiGroupServerMod,     only: MultiGroupServer
   use pFIO_BaseServerMod,           only: BaseServer
   use mapl_ErrorHandling_mod
   use esmf
   implicit none
   private

   public :: setServices

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

      ! Server GridComps register only a run phase — no init, no finalize.
      call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, rc=status)
      rc = status
   end subroutine setServices

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)  :: gridcomp
      type(ESMF_State)     :: importState
      type(ESMF_State)     :: exportState
      type(ESMF_Clock)     :: clock
      integer, intent(out) :: rc

      type(ServerResources), pointer :: resources
      character(ESMF_MAXSTR) :: server_name
      class(BaseServer), allocatable, target :: server
      integer :: status

      ! GridComp name doubles as the port/server name in the DirectoryService.
      call ESMF_GridCompGet(gridcomp, name=server_name, _RC)

      ! Retrieve comms + subclass stored by make_server_gridcomp.
      _GET_NAMED_PRIVATE_STATE(gridcomp, ServerResources, 'private state', resources)

      select case (trim(resources%subclass))
      case ('MpiServer', '')
         allocate(server, source=MpiServer(resources%server_comm, trim(server_name)))
      case ('MultiGroupServer')
         allocate(server, source=MultiGroupServer(resources%server_comm, trim(server_name), &
              nwriter_per_node=resources%nwriter_per_node))
      case default
         _FAIL('Unknown server subclass: ' // trim(resources%subclass))
      end select

      ! Publish on the MAPL singleton DirectoryService; blocks until client connects.
      call MAPL_PublishServer(trim(server_name), server, _RC)

      ! Serve until client terminates.
      call server%start(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module mapl_PfioServerGridComp_mod

! Standalone entry point so ESMF can resolve the symbol via dlsym when this
! library is specified as sharedObj in a server YAML entry.
! The module procedure is not directly reachable by simple name mangling
! across all compilers; this thin wrapper is.
subroutine setServices(gridcomp, rc)
   use ESMF
   use mapl_PfioServerGridComp_mod, only: pfio_server_setServices => setServices
   type(ESMF_GridComp) :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call pfio_server_setServices(gridcomp, status)
   rc = status
end subroutine setServices
