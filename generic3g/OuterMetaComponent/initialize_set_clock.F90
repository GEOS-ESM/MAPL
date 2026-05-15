#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_set_clock_smod
   use mapl3g_GenericPhases, only: GENERIC_INIT_SET_CLOCK
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriverMap
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
      type(ESMF_TimeInterval) :: timeStep, user_timeStep, user_offset
      logical :: compatible
      
      call ESMF_ClockGet(outer_clock, timeStep=timeStep, _RC)

      user_timeStep = timeStep
      if (allocated(this%user_timeStep)) user_timeStep = this%user_timeStep
      this%user_timeStep = user_timeStep

      user_offset = this%user_offset

      call check_compatibility(user_timestep, timeStep, compatible, offset=user_offset, _RC)
      _ASSERT(compatible, 'The user timestep and offset are not compatible with the outer timestep.')

      user_clock = ESMF_ClockCreate(outer_clock, _RC)
      call ESMF_ClockSet(user_clock, timestep=user_timeStep, _RC)
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
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Clock), intent(in) :: outer_clock
      type(ESMF_Clock), intent(inout) :: user_clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TimeInterval) :: outer_timestep, user_timestep
      type(ESMF_Time) :: currTime, clock_refTime, user_runTime, startTime, user_clockTime
      logical :: has_run_next_step, has_ref_datetime
      character(len=:), allocatable :: ref_datetime

      call ESMF_ClockGet(outer_clock, timestep=outer_timestep, currTime=currTime, refTime=clock_refTime, startTime=startTime, _RC)
      call ESMF_ClockGet(user_clock, timestep=user_timestep, _RC)

      has_ref_datetime = ESMF_HConfigIsDefined(this%hconfig, keyString='ref_datetime', _RC)
      has_run_next_step = ESMF_HConfigIsDefined(this%hconfig, keyString='run_next_step', _RC)

      ! this logic for ref_datetime is there to set the alarm right in the case of a ref_datetime
      ! as well as to set the clock correctly for components that use a ref_datetime
      if (has_ref_datetime) then
         ! if we have ref_datetime must sub this into current time to set initial ring time
         ref_datetime = ESMF_HConfigAsString(this%hconfig, keyString='ref_datetime', _RC)
         user_runTime = sub_time_in_datetime(currTime, ref_datetime, _RC)
         user_clockTime = user_runTime
      else
         user_runTime = clock_refTime + this%user_offset
      end if

      this%run_if_alarm_rings_next = .false.
      if (has_run_next_step) then
         this%run_if_alarm_rings_next = ESMF_HConfigAsLogical(this%hconfig, keyString='run_next_step', _RC)
      end if

      this%user_run_alarm = SimpleAlarm(user_runTime, user_timeStep, _RC)
      if (has_ref_datetime) then
         ! want to shift it back until user_clockTime is greater OR equal to start time of clock
         ! this gets the clock back to time closest to the start of that clock
         ! and still compatible with the reference time
         call reset_user_time(user_clockTime, currTime, user_timestep, _RC)
         call ESMF_ClockGet(user_clock, startTime=startTime, _RC)
         if (startTime > user_clockTime) then
            call ESMF_ClockSet(user_clock, startTime=user_clockTime, _RC)
         end if
         call ESMF_ClockSet(user_clock, currTime=user_clockTime, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine set_run_user_alarm

   subroutine reset_user_time(user_clockTime, currTime, user_timeStep, rc)
         type(ESMF_Time), intent(inout) :: user_clockTime
         type(ESMF_Time), intent(in) :: currTime
         type(ESMF_TimeInterval), intent(in) :: user_timeStep
         integer, optional, intent(out) :: rc

         type(ESMF_Time) :: temp_time

         temp_time = user_clockTime
         if (user_clockTime < currTime) then
            do while(temp_time < currTime)
               temp_time=temp_time+user_timeStep
            enddo
            if (temp_time /= currTime) temp_time=temp_time-user_timeStep
         else if (user_clockTime > currTime) then
            do while(temp_time > currTime)
               temp_time=temp_time-user_timeStep
            enddo
         end if
         user_clockTime=temp_time
 
         _RETURN(_SUCCESS)

   end subroutine reset_user_time

end submodule initialize_set_clock_smod
