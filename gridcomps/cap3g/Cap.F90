#include "MAPL_Generic.h"

module mapl3g_Cap
   use mapl3g_CapGridComp, only: cap_setservices => setServices
   use generic3g
   use mapl3g_GenericPhases
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: MAPL_run_driver

contains


   subroutine MAPL_run_driver(hconfig, unusable, rc)
      type(ESMF_HConfig), intent(inout) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver) :: driver
      integer :: status

      driver = make_driver(hconfig, _RC)

      call initialize_phases(driver, phases=GENERIC_INIT_PHASE_SEQUENCE, _RC)
      call integrate(driver, _RC)
      call driver%finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine MAPL_run_driver

   function make_driver(hconfig, rc) result(driver)
      type(GriddedComponentDriver) :: driver
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc

      type(ESMF_GridComp) :: cap_gridcomp
      type(ESMF_Clock) :: clock
      character(:), allocatable :: cap_name
      integer :: status

      cap_name = ESMF_HConfigAsString(hconfig, keystring='cap_name', _RC)
      clock = create_clock(hconfig, _RC)
      ! TODO:  Rename to MAPL_CreateGridComp() ?
      cap_gridcomp = create_grid_comp(cap_name, user_setservices(cap_setservices), hconfig, _RC)
      driver = GriddedComponentDriver(cap_gridcomp, clock=clock)

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

      call set_time(startTime, 'start', clock_config, _RC)
      call set_time(stopTime, 'stop', clock_config, _RC)
      call set_time_interval(timeStep, 'dt', clock_config, _RC)
      call set_time_interval(segment_duration, 'segment_duration', clock_config, _RC)

      end_of_segment = startTime + segment_duration
      if (end_of_segment < stopTime) stopTime = end_of_segment
      clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, stopTime=stopTime, _RC)
      
      _RETURN(_SUCCESS)
   end function create_clock

   subroutine set_time_interval(interval, key, hconfig, rc)
      type(ESMF_TimeInterval), intent(out) :: interval
      character(*), intent(in) :: key
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: iso_duration
      
      iso_duration = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
!#      call ESMF_TimeIntervalSet(interval, timeString=iso_duration, _RC)
      
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

      _RETURN(_SUCCESS)
      
   end subroutine integrate

end module mapl3g_Cap
