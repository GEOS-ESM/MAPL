#include "MAPL_Generic.h"

module mapl3g_FakeGocartGridComp

   use mapl_ErrorHandling
   use mapl3g_generic, only: MAPL_GridCompSetEntryPoint, MAPL_GridCompRunChildren
   use esmf

   implicit none
   private

   public :: SetServices

contains

   subroutine SetServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="fake-gocart-run", _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      print *, "RUNNING FAKE GOCART"
      ! Children with 2 run phases
      call MAPL_GridCompRunChildren(gridcomp, phase_name="Run", _RC)
      call MAPL_GridCompRunChildren(gridcomp, phase_name="Run2", _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module mapl3g_FakeGocartGridComp

subroutine SetServices(gridcomp, rc)
   use MAPL_ErrorHandlingMod
   use mapl3g_FakeGocartGridComp, only: FakeGocart_SetServices => SetServices
   use esmf

   type(ESMF_GridComp), intent(inout)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call FakeGocart_SetServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine SetServices
