#include "MAPL_Generic.h"

module ConfigurableGridComp

   use mapl_ErrorHandling
   use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint, MAPL_RunChildren
   use esmf

   implicit none
   private

   public :: setServices

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine init

   recursive subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      call MAPL_RunChildren(gridcomp, phase_name="run", _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module ConfigurableGridComp

subroutine setServices(gridcomp, rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use ConfigurableGridComp, only: Configurable_setServices => SetServices
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call Configurable_setServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices
