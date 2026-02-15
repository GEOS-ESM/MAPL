#include "MAPL.h"

module mapl3g_FakeParentGridComp

   use mapl_ErrorHandling
   use mapl3g_generic, only: MAPL_GridCompSetEntryPoint
   use mapl3g_generic, only: MAPL_GridCompGet
   use mapl3g_generic, only: MAPL_GridCompRunChildren
   use pflogger, only: logger_t => logger
   use esmf

   implicit none
   private

   public :: SetServices

contains

   subroutine SetServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      class(logger_t), pointer :: logger
      integer :: status

      call MAPL_GridCompGet(gridcomp, logger=logger, _RC)
      call logger%info("SetServices")

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine run(gridcomp, import_state, export_state, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      class(Logger_t), pointer :: logger
      integer :: status

      call MAPL_GridCompGet(gridcomp, logger=logger, _RC)
      call logger%info("Run: starting...")
      ! Children with 2 run phases
      call MAPL_GridCompRunChildren(gridcomp, phase_name="Run1", _RC)
      call MAPL_GridCompRunChildren(gridcomp, phase_name="Run2", _RC)
      call logger%info("Run: ...complete")

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(import_state)
      _UNUSED_DUMMY(export_state)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module mapl3g_FakeParentGridComp

subroutine SetServices(gridcomp, rc)
   use MAPL_ErrorHandlingMod
   use mapl3g_FakeParentGridComp, only: FakeParent_SetServices => SetServices
   use esmf

   type(ESMF_GridComp), intent(inout)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call FakeParent_SetServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine SetServices
