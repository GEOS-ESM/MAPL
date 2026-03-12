#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_set_clock_smod
   use mapl3g_GenericPhases, only: GENERIC_INIT_SET_CLOCK
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriverMap
   use mapl3g_ESMF_Time_Utilities
   use mapl_ErrorHandling
   use mapl3g_HConfig_API
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

      !call check_compatibility(user_timestep, timeStep, compatible, offset=user_offset, _RC)
      !_ASSERT(compatible, 'The user timestep and offset are not compatible with the outer timestep.')

      user_clock = ESMF_ClockCreate(outer_clock, _RC)
      call ESMF_ClockSet(user_clock, timestep=user_timeStep, _RC)
      call set_run_user_alarm(this, outer_clock, user_clock, _RC)

      block
         type(ESMF_Time) :: current_time
         character(len=ESMF_MAXSTR) :: time_string
         call ESMF_ClockGet(user_clock, currTime=current_time, _RC)
         call ESMF_TimeGet(current_time, timeString=time_string, _RC)
      end block
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
      type(ESMF_TimeInterval) :: outer_timestep, user_timestep, ref_time, t24
      type(ESMF_Time) :: currTime, clock_refTime, user_runTime, startTime, user_clockTime
      logical :: has_run_next_step, has_ref_time, run_next_step

      call ESMF_ClockGet(outer_clock, timestep=outer_timestep, currTime=currTime, refTime=clock_refTime, startTime=startTime, _RC)
      call ESMF_ClockGet(user_clock, timestep=user_timestep, _RC)

      has_ref_time = ESMF_HConfigIsDefined(this%hconfig, keyString='ref_time', _RC)
      has_run_next_step = ESMF_HConfigIsDefined(this%hconfig, keyString='run_next_step', _RC)

      if (has_ref_time) then
         ! if we have ref_time must sub this into current time to set initial ring time
         ! also the clock must be also adjusted so that it starts on the ref time
         ref_time = MAPL_HConfigAsTimeInterval(this%hconfig, keyString='ref_time', _RC) 
         call ESMF_TimeIntervalSet(t24, h=24, _RC)
         _ASSERT(ref_time <= t24, 'reference time must be between 0 and 24 hours')
         user_runTime = sub_time_in_datetime(currTime, ref_time, _RC)
         user_clockTime = user_runTime
      else
         user_runTime = clock_refTime + this%user_offset
      end if

      this%run_if_alarm_rings_next = .false.
      if (has_run_next_step) then
         this%run_if_alarm_rings_next = ESMF_HConfigAsLogical(this%hconfig, keyString='run_next_step', _RC)
      end if

      this%user_run_alarm = SimpleAlarm(user_runTime, user_timeStep, _RC)
      if (has_ref_time) then 
         ! want to shift it back until user_clockTime is greater OR equal to start time of clock
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
         do while(temp_time >= currTime)
            temp_time=temp_time-user_timeStep
         enddo
         if (temp_time < currTime) temp_time=temp_time+user_timeStep
         user_clockTime = temp_time
 
         _RETURN(_SUCCESS)

   end subroutine reset_user_time

   function sub_time_in_datetime(time, time_interval, rc) result(new_time)
      type(ESMF_Time) :: new_time
      type(ESMF_Time), intent(in) :: time
      type(ESMF_TimeInterval), intent(in) :: time_interval
      integer, optional, intent(out) :: rc

      integer :: status, year, month, day

      call ESMF_TimeGet(time, yy=year, mm=month, dd=day, _RC)
      call ESMF_TimeSet(new_time, yy=year, mm=month, dd=day, h=0, m=0, s=0, _RC)
      new_time=new_time+time_interval

      _RETURN(_SUCCESS)
   end function sub_time_in_datetime

end submodule initialize_set_clock_smod
