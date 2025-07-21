#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) run_clock_advance_smod
   use mapl3g_GenericPhases
   use mapl3g_GriddedComponentDriverMap
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine run_clock_advance(this, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(StringVector), pointer :: run_phases
      logical :: found
      type(ESMF_Alarm) :: alarm
      logical :: is_ringing
      integer :: phase

      call ESMF_ClockGetAlarm(clock, alarm=alarm, alarmName=RUN_USER_ALARM, _RC)
      is_ringing = ESMF_AlarmIsRinging(alarm, _RC)
      _RETURN_IF(.not. is_ringing)

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           child => iter%second()
           call child%run(phase_idx=GENERIC_RUN_CLOCK_ADVANCE, _RC)
           call child%clock_advance()
        end do
      end associate

      call this%user_gc_driver%clock_advance(_RC)

      ! Check for customization
      run_phases => this%get_phases(ESMF_METHOD_RUN)
      phase = get_phase_index(run_phases, phase_name='GENERIC::RUN_CLOCK_ADVANCE', found=found)
      if (found) then
         call this%user_gc_driver%run(phase_idx=phase, _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine run_clock_advance

end submodule run_clock_advance_smod
