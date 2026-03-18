#include "MAPL.h"

module mapl3g_FakeParentGridComp

   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Assert, MAPL_Return
   use mapl3g_generic, only: MAPL_GridCompSetEntryPoint
   use mapl3g_generic, only: MAPL_GridCompGet
   use mapl3g_generic, only: MAPL_GridCompRunChildren
   use ESMF, only: ESMF_GridComp, ESMF_State, ESMF_Clock, ESMF_METHOD_RUN
   use ESMF, only: ESMF_HConfig, ESMF_HConfigIsDefined, ESMF_HConfigAsLogical
   use ESMF, only: ESMF_HConfigCreateAt, ESMF_HConfigDestroy

   implicit none
   private

   public :: SetServices

contains

   subroutine SetServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

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

      integer :: status
      logical :: run1, run2
      type(ESMF_HConfig) :: hconfig

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      call get_run_phases_(hconfig, run1, run2, _RC)
      if (run1) call MAPL_GridCompRunChildren(gridcomp, phase_name='Run1', _RC)
      if (run2) call MAPL_GridCompRunChildren(gridcomp, phase_name='Run2', _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(import_state)
      _UNUSED_DUMMY(export_state)
      _UNUSED_DUMMY(clock)
   end subroutine run

   subroutine get_run_phases_(hconfig, run1, run2, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      logical, intent(out) :: run1, run2
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: mapl_hconfig, run_phases_hconfig
      logical :: has_mapl, has_run_phases, has_run1, has_run2

      run1 = .false. ! default to running only phase 2
      run2 = .true.

      has_mapl = ESMF_HConfigIsDefined(hconfig, keyString='mapl', _RC)
      _ASSERT(has_mapl, 'No mapl section found in hconfig for FakeParentGridComp')

      mapl_hconfig = ESMF_HConfigCreateAt(hconfig, keyString='mapl', _RC)
      has_run_phases = ESMF_HConfigIsDefined(mapl_hconfig, keyString='run_phases', _RC)
      if (has_run_phases) then
         run_phases_hconfig = ESMF_HConfigCreateAt(mapl_hconfig, keyString='run_phases', _RC)
         has_run1 = ESMF_HConfigIsDefined(run_phases_hconfig, keyString='run1', _RC)
         if (has_run1) run1 = ESMF_HConfigAsLogical(run_phases_hconfig, keyString='run1', _RC)
         has_run2 = ESMF_HConfigIsDefined(run_phases_hconfig, keyString='run2', _RC)
         if (has_run2) run2 = ESMF_HConfigAsLogical(run_phases_hconfig, keyString='run2', _RC)
         call ESMF_HConfigDestroy(run_phases_hconfig, _RC)
      end if
      call ESMF_HConfigDestroy(mapl_hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_run_phases_

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
