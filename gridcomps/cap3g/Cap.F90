#include "MAPL_Generic.h"

module mapl3g_Cap
   use mapl3g_CapGridComp, only: cap_setservices => setServices
   use generic3g
   use mapl3g_GenericPhases
   use mapl3g_MultiState
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: MAPL_run_driver

contains


   subroutine MAPL_run_driver(hconfig, unusable, rc)
      USE MAPL_ApplicationSupport
      type(ESMF_HConfig), intent(inout) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver) :: driver
      integer :: status

      driver = make_driver(hconfig, _RC)

       _HERE
     call initialize_phases(driver, phases=GENERIC_INIT_PHASE_SEQUENCE, _RC)
      _HERE
      call integrate(driver, _RC)
      _HERE
      call driver%finalize(_RC)
      _HERE

      _RETURN(_SUCCESS)
   end subroutine MAPL_run_driver

   function make_driver(hconfig, rc) result(driver)
      use mapl3g_GenericGridComp, only: generic_SetServices => setServices
      type(GriddedComponentDriver) :: driver
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc

      type(ESMF_GridComp) :: cap_gridcomp
      type(ESMF_Clock) :: clock
      character(:), allocatable :: cap_name
      integer :: status, user_status

      _HERE
      cap_name = ESMF_HConfigAsString(hconfig, keystring='cap_name', _RC)
      ! TODO:  Rename to MAPL_CreateGridComp() ?
      clock = create_clock(hconfig, _RC)
      cap_gridcomp = create_grid_comp(cap_name, user_setservices(cap_setservices), hconfig, clock, _RC)
      call ESMF_GridCompSetServices(cap_gridcomp, generic_setServices, userRC=user_status, _RC)
      _VERIFY(user_status)

      _HERE
      driver = GriddedComponentDriver(cap_gridcomp, clock, MultiState())
      _HERE

      _RETURN(_SUCCESS)
   end function make_driver

   function create_clock(hconfig, rc) result(clock)
      type(ESMF_Clock) :: clock
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Time) :: startTime, stopTime, end_of_segment
      type(ESMF_TimeInterval) :: timeStep, segment_duration
      type(ESMF_HConfig) :: clock_config
      
      clock_config = ESMF_HConfigCreateAt(hconfig, keystring='clock', _RC)

      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN,_RC)
      call set_time(startTime, 'start', clock_config, _RC)
      call ESMF_TimePrint(startTime, options='string', prestring='start time set: ' ,_RC)
      call set_time(stopTime, 'stop', clock_config, _RC)
      call ESMF_TimePrint(stopTime, options='string', prestring='stop time set: ', _RC)
      call set_time_interval(timeStep, 'dt', clock_config, _RC)
      call set_time_interval(segment_duration, 'segment_duration', clock_config, _RC)

      end_of_segment = startTime + segment_duration
      if (end_of_segment < stopTime) stopTime = end_of_segment
      call ESMF_TimePrint(stopTime, options='string', prestring='actual stop time set: ', _RC)
      clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, stopTime=stopTime, _RC)
      
      _RETURN(_SUCCESS)
   end function create_clock

   subroutine set_time_interval(interval, key, hconfig, rc)
      type(ESMF_TimeInterval), intent(out) :: interval
      character(*), intent(in) :: key
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      
      integer :: status

      integer :: strlen,ppos,cpos,lpos,tpos
      integer year,month,day,hour,min,sec
      character(len=:), allocatable :: date_string,time_string
      character(:), allocatable :: iso_duration
      
      iso_duration = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
!#      call ESMF_TimeIntervalSet(interval, timeString=iso_duration, _RC)
      year=0
      month=0
      day=0
      hour=0
      min=0
      sec=0
      strlen = len_trim(iso_duration)
      tpos = index(iso_duration,'T')
      ppos = index(iso_duration,'P')
      _ASSERT(iso_duration(1:1) == 'P','Not valid time duration')

      if (tpos /= 0) then
         if (tpos /= ppos+1) then
            date_string = iso_duration(ppos+1:tpos-1)
         end if
         time_string = iso_duration(tpos+1:strlen)
      else
         date_string = iso_duration(ppos+1:strlen)
      end if

      if (allocated(date_string)) then
         strlen = len_trim(date_string)
         lpos = 0
         cpos = index(date_string,'Y')
         if (cpos /= 0) then
            read(date_string(lpos+1:cpos-1),*)year
            lpos = cpos
         end if
         cpos = index(date_string,'M')
         if (cpos /= 0) then
            read(date_string(lpos+1:cpos-1),*)month
            lpos = cpos
         end if
         cpos = index(date_string,'D')
         if (cpos /= 0) then
            read(date_string(lpos+1:cpos-1),*)day
            lpos = cpos
         end if
      end if      
      if (allocated(time_string)) then
         strlen = len_trim(time_string)
         lpos = 0
         cpos = index(time_string,'H')
         if (cpos /= 0) then
            read(time_string(lpos+1:cpos-1),*)hour
            lpos = cpos
         end if
         cpos = index(time_string,'M')
         if (cpos /= 0) then
            read(time_string(lpos+1:cpos-1),*)min
            lpos = cpos
         end if
         cpos = index(time_string,'S')
         if (cpos /= 0) then
            read(time_string(lpos+1:cpos-1),*)sec
            lpos = cpos
         end if
      end if
      call ESMF_TimeIntervalSet(interval, yy=year, mm=month, d=day, h=hour, m=min, s=sec,_RC) 
      _RETURN(_SUCCESS)
   end subroutine set_time_interval

   subroutine set_time(time, key, hconfig, rc)
      type(ESMF_Time), intent(out) :: time
      character(*), intent(in) :: key
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: iso_time
      
      iso_time = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
      call ESMF_TimeSet(time, timeString=iso_time, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine set_time

   subroutine integrate(driver, rc)
      type(GriddedComponentDriver), intent(inout) :: driver
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime, stopTime
      
      clock = driver%get_clock()
      call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, _RC)

      do while (currTime < stopTime)
         ! TODO:  include Bill's monitoring log messages here
         call driver%run(_RC)
         call ESMF_ClockAdvance(clock, _RC)
         call ESMF_ClockGet(clock, currTime=currTime, _RC)
      end do
      call ESMF_TimePrint(currTime, options='string', preString='Cap time after loop: ', _RC)

      _RETURN(_SUCCESS)
      
   end subroutine integrate

end module mapl3g_Cap
