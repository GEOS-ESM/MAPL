#include "MAPL_Generic.h"

module mapl3g_Cap
   use mapl3
   use mapl3g_CapGridComp, only: cap_setservices => setServices
   use MAPL_TimeStringConversion, only: hconfig_to_esmf_timeinterval
   use esmf, only: ESMF_GridCompSetServices
   implicit none
   private

   public :: MAPL_run_driver

contains


   subroutine MAPL_run_driver(hconfig, is_model_pet, unusable, servers, rc)
      USE MAPL_ApplicationSupport
      type(ESMF_HConfig), intent(inout) :: hconfig
      logical, intent(in) :: is_model_pet
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_GridComp), optional, intent(in) :: servers(:)
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver) :: driver
      integer :: status

      driver = make_driver(hconfig, is_model_pet, _RC)

      if (is_model_pet) then
         call initialize_phases(driver, phases=GENERIC_INIT_PHASE_SEQUENCE, _RC)
         call integrate(driver, _RC)
         call driver%write_restart(_RC)
         call driver%finalize(_RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine MAPL_run_driver

   function make_driver(hconfig, is_model_pet, rc) result(driver)
      use mapl3g_GenericGridComp, only: generic_SetServices => setServices
      type(GriddedComponentDriver) :: driver
      type(ESMF_HConfig), intent(inout) :: hconfig
      logical, intent(in) :: is_model_pet
      integer, optional, intent(out) :: rc

      type(ESMF_GridComp) :: cap_gridcomp
      type(ESMF_Clock) :: clock
      character(:), allocatable :: cap_name
      integer :: status, user_status
      type(ESMF_HConfig) :: cap_gc_hconfig
      integer, allocatable :: petList(:)

      cap_name = ESMF_HConfigAsString(hconfig, keystring='name', _RC)
      clock = create_clock(hconfig, _RC)

      cap_gc_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='cap_gc', _RC)
      petList = get_model_pets(is_model_pet, _RC)
      cap_gridcomp = MAPL_GridCompCreate(cap_name, user_setservices(cap_setservices), cap_gc_hconfig, petList=petList, _RC)

      call ESMF_GridCompSetServices(cap_gridcomp, generic_setServices, userRC=user_status, _RC)
      _VERIFY(user_status)

      driver = GriddedComponentDriver(cap_gridcomp, clock=clock)

      _RETURN(_SUCCESS)
   end function make_driver

   ! Create function that accepts a logical flag returns list of mpi processes that have .true..
   function get_model_pets(flag, rc) result(petList)
      use mpi
      integer, allocatable :: petList(:)
      logical, intent(in) :: flag
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_VM) :: vm
      logical, allocatable, target :: flags(:)
      integer :: world_comm
      integer :: i, petCount
      
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, mpiCommunicator=world_comm, _RC)
      allocate(flags(petCount))
      call MPI_Allgather(flag, 1, MPI_LOGICAL, flags, 1, MPI_LOGICAL, world_comm, status)
      _VERIFY(status)
      petList = pack([(i, i=0,petCount-1)], flags)

      _RETURN(_SUCCESS)
   end function get_model_pets

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
      timeStep = hconfig_to_esmf_timeinterval(clock_config, 'dt', _RC)
      segment_duration = hconfig_to_esmf_timeinterval(clock_config, 'segment_duration', _RC)

      end_of_segment = startTime + segment_duration
      if (end_of_segment < stopTime) stopTime = end_of_segment
      call ESMF_TimePrint(stopTime, options='string', prestring='actual stop time set: ', _RC)
      clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, stopTime=stopTime, refTime=startTime, _RC)
      
      _RETURN(_SUCCESS)
   end function create_clock

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
         call driver%run(phase_idx=GENERIC_RUN_USER, _RC)
         call driver%run(phase_idx=GENERIC_RUN_CLOCK_ADVANCE, _RC)
         call driver%clock_advance(_RC)
         call ESMF_ClockGet(clock, currTime=currTime, _RC)
      end do
      call esmf_TimePrint(currTime, options='string', preString='Cap time after loop: ', _RC)

      _RETURN(_SUCCESS)
      
   end subroutine integrate

end module mapl3g_Cap
