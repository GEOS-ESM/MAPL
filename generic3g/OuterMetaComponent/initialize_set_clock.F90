#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_set_clock_smod
   use mapl3g_GenericPhases, only: GENERIC_INIT_SET_CLOCK
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriverMap
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_INVALIDATE, GENERIC_COUPLER_UPDATE
   use mapl3g_ESMF_Time_Utilities
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   ! User clock can run at a "multiple" of the outer
   ! clock. ("multiple" is in quotes because steps of a month also are
   ! allowed so long as the outer timestep divides evenly into a day.)

   ! The clock of the outer meta each child should use the same
   ! parameters
   module recursive subroutine initialize_set_clock(this, outer_clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_Clock), intent(in) :: outer_clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Clock) :: user_clock
      type(ESMF_Time) :: user_refTime, default_refTime
      type(ESMF_TimeInterval) :: user_timeStep, default_timeStep
      logical :: compatible
      

      call ESMF_ClockGet(outer_clock, timeStep=default_timeStep, refTime=default_refTime, _RC)

      user_timeStep = default_timeStep
      if (allocated(this%user_timeStep)) user_timeStep = this%user_timeStep

      user_refTime = default_refTime
      if (allocated(this%user_refTime)) user_refTime = this%user_refTime
  
      call times_and_intervals_are_compatible(user_timestep, user_refTime, default_timestep, default_refTime, compatible, _RC)
      _ASSERT(compatible, 'The user timestep and refTime are not compatible with the outer timestep and refTime')

      user_clock = ESMF_ClockCreate(outer_clock, _RC)
      call ESMF_ClockSet(user_clock, timestep=user_timeStep, reftime=user_refTime, _RC)
      call set_run_user_alarm(this, outer_clock, user_clock, _RC)

      call this%user_gc_driver%set_clock(user_clock)

      call set_children_outer_clock(this%children, user_clock, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_SET_CLOCK, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   contains


      subroutine set_children_outer_clock(children, clock, rc)
         type(GriddedComponentDriverMap), target, intent(inout) :: children
         type(ESMF_Clock), intent(in) :: clock
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Clock) :: child_clock
         type(GriddedComponentDriverMapIterator) :: iter
         type(GriddedComponentDriver), pointer :: child_driver

         iter = children%ftn_begin()
         associate (e => children%ftn_end())
           do while (iter /= e)
              call iter%next()
              child_clock = ESMF_ClockCreate(clock, _RC)
              child_driver => iter%second()
              call child_driver%set_clock(child_clock)
           end do
         end associate
      
         _RETURN(ESMF_SUCCESS)
      end subroutine set_children_outer_clock

   end subroutine initialize_set_clock

  subroutine set_run_user_alarm(this, outer_clock, user_clock,  rc)
      class(OuterMetaComponent), intent(in) :: this
      type(ESMF_Clock), intent(in) :: outer_clock
      type(ESMF_Clock), intent(in) :: user_clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TimeInterval) :: outer_timestep, user_timestep, zero
      type(ESMF_Time) :: user_refTime
      type(ESMF_Time) :: currTime
      type(ESMF_Alarm) :: alarm

      call ESMF_TimeIntervalSet(zero, s=0, _RC)

      call ESMF_ClockGet(outer_clock, timestep=outer_timestep, currTime=currTime, _RC)
      call ESMF_ClockGet(user_clock, timestep=user_timestep, refTime=user_refTime, _RC)

      alarm = ESMF_AlarmCreate(outer_clock, &
           name = RUN_USER_ALARM, &
           ringInterval=user_timestep, &
           ringTime=user_refTime, &
           sticky=.false., &
           _RC)

      if (user_refTime < currTime) then
         call ESMF_AlarmRingerOff(alarm, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine set_run_user_alarm

end submodule initialize_set_clock_smod
