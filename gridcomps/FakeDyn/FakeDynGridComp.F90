#include "MAPL_Generic.h"

module mapl3g_FakeDynGridComp

   use generic3g
   use mapl_ErrorHandling
   use pFlogger, only: logger
   use esmf

   implicit none
   private

   public :: SetServices

contains

   subroutine SetServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig, collections_config, child_hconfig
      character(len=:), allocatable :: child_name, collection_name
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      logical :: has_active_collections
      class(logger), pointer :: lgr
      integer :: num_collections, status

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_USER", _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)

      ! Determine collections
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine init

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status

      call MAPL_RunChildren(gridcomp, phase_name="run", _RC)
      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_FakeDynGridComp

subroutine SetServices(gridcomp,rc)
   use MAPL_ErrorHandlingMod
   use mapl3g_FakeDynGridComp, only: FakeDyn_SetServices => SetServices
   use esmf

   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call FakeDyn_SetServices(gridcomp,_RC)
   _RETURN(_SUCCESS)
end subroutine SetServices
