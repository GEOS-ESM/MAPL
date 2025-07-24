#include "MAPL.h"

module mapl3g_Cap
   use mapl3
   use mapl3g_CapGridComp, only: cap_setservices => setServices
   use mapl_TimeStringConversion, only: hconfig_to_esmf_timeinterval
   use mapl_os
   use esmf
   implicit none
   private

   public :: mapl_run_driver

   character(*), parameter :: RECORD_ALARM = 'record'
   character(*), parameter :: CHECKPOINTS_DIR = 'checkpoints'
   character(*), parameter :: COLLECTIONS_DIR = 'collections'
   character(*), parameter :: LOGS_DIR = 'logs'

contains


   subroutine mapl_run_driver(hconfig, is_model_pet, unusable, servers, rc)
      USE mapl_ApplicationSupport
      type(esmf_HConfig), intent(inout) :: hconfig
      logical, intent(in) :: is_model_pet
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), optional, intent(in) :: servers(:)
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver) :: driver
      integer :: status

      driver = make_driver(hconfig, is_model_pet, _RC)
      call make_directories(_RC)

      if (is_model_pet) then
         call initialize_phases(driver, phases=GENERIC_INIT_PHASE_SEQUENCE, _RC)
         call integrate(driver, _RC)
         call driver%finalize(_RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_run_driver

   function make_driver(hconfig, is_model_pet, rc) result(driver)
      use mapl3g_GenericGridComp, only: generic_SetServices => setServices
      type(GriddedComponentDriver) :: driver
      type(esmf_HConfig), intent(inout) :: hconfig
      logical, intent(in) :: is_model_pet
      integer, optional, intent(out) :: rc

      type(esmf_GridComp) :: cap_gridcomp
      type(esmf_Clock) :: clock
      character(:), allocatable :: cap_name
      integer :: status, user_status
      type(esmf_HConfig) :: cap_gc_hconfig
      integer, allocatable :: petList(:)

      cap_name = esmf_HConfigAsString(hconfig, keystring='name', _RC)
      clock = create_clock(hconfig, _RC)

      cap_gc_hconfig = esmf_HConfigCreateAt(hconfig, keystring='cap_gc', _RC)
      petList = get_model_pets(is_model_pet, _RC)
      cap_gridcomp = mapl_GridCompCreate(cap_name, user_setservices(cap_setservices), cap_gc_hconfig, petList=petList, _RC)

      call esmf_GridCompSetServices(cap_gridcomp, generic_setServices, userRC=user_status, _RC)
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
      type(esmf_VM) :: vm
      logical, allocatable, target :: flags(:)
      integer :: world_comm
      integer :: i, petCount

      call esmf_VMGetCurrent(vm, _RC)
      call esmf_VMGet(vm, petCount=petCount, mpiCommunicator=world_comm, _RC)
      allocate(flags(petCount))
      call MPI_Allgather(flag, 1, MPI_LOGICAL, flags, 1, MPI_LOGICAL, world_comm, status)
      _VERIFY(status)
      petList = pack([(i, i=0,petCount-1)], flags)

      _RETURN(_SUCCESS)
   end function get_model_pets

   function create_clock(hconfig, rc) result(clock)
      type(esmf_Clock) :: clock
      type(esmf_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Time) :: startTime, stopTime, end_of_segment
      type(esmf_TimeInterval) :: timeStep, segment_duration
      type(esmf_HConfig) :: clock_config

      clock_config = esmf_HConfigCreateAt(hconfig, keystring='clock', _RC)

      call esmf_CalendarSetDefault(esmf_CALKIND_GREGORIAN,_RC)
      call set_time(startTime, 'start', clock_config, _RC)
      call esmf_TimePrint(startTime, options='string', prestring='start time set: ' ,_RC)
      call set_time(stopTime, 'stop', clock_config, _RC)
      call esmf_TimePrint(stopTime, options='string', prestring='stop time set: ', _RC)
      timeStep = hconfig_to_esmf_timeinterval(clock_config, 'dt', _RC)
      segment_duration = hconfig_to_esmf_timeinterval(clock_config, 'segment_duration', _RC)

      end_of_segment = startTime + segment_duration
      if (end_of_segment < stopTime) stopTime = end_of_segment
      call esmf_TimePrint(stopTime, options='string', prestring='actual stop time set: ', _RC)
      clock = esmf_ClockCreate(timeStep=timeStep, startTime=startTime, stopTime=stopTime, refTime=startTime, _RC)

      call add_record_alarm(clock, hconfig, _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine add_record_alarm(clock, cfg, rc)
         type(esmf_Clock), intent(inout) :: clock
         type(esmf_HConfig), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_record_section, has_offset
         type(esmf_HConfig) :: record_cfg
         type(esmf_TimeInterval) :: record_frequency, record_offset
         type(esmf_Time) :: ringTime
         type(esmf_Alarm) :: alarm

         has_record_section = esmf_HConfigIsDefined(cfg, keystring='record', _RC)
         if (.not. has_record_section) then
            ! We want a default alarm that never rings.  ESMF forces a bit more info to be specified.
            alarm = esmf_AlarmCreate(clock, enabled=.false., name=RECORD_ALARM, ringTime=startTime,_RC)
            _RETURN(_SUCCESS)
         end if

         record_cfg = esmf_HConfigCreateAt(hconfig, keyString='record', _RC)
         record_frequency = hconfig_to_esmf_timeinterval(record_cfg, 'frequency', _RC)

         has_offset = esmf_HConfigIsDefined(record_cfg, keystring='offset', _RC)
         if (has_offset) then
            record_offset = hconfig_to_esmf_timeinterval(record_cfg, 'offset', _RC)
         end if
         call esmf_HConfigDestroy(record_cfg, _RC)

         ringTime = startTime + record_offset
         alarm = esmf_AlarmCreate(clock, name=RECORD_ALARM, ringTime=ringTime, ringInterval=record_frequency, sticky=.false., _RC)

         _RETURN(_SUCCESS)
      end subroutine add_record_alarm

   end function create_clock

   subroutine set_time(time, key, hconfig, rc)
      type(esmf_Time), intent(out) :: time
      character(*), intent(in) :: key
      type(esmf_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: iso_time

      iso_time = esmf_HConfigAsString(hconfig, keystring=key, _RC)
      call esmf_TimeSet(time, timeString=iso_time, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_time

   subroutine integrate(driver, rc)
      type(GriddedComponentDriver), intent(inout) :: driver
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Clock) :: clock
      type(esmf_Time) :: currTime, stopTime
      type(esmf_Alarm) :: alarm
      logical :: is_record_time

      clock = driver%get_clock()

      call esmf_ClockGet(clock, currTime=currTime, stopTime=stopTime, _RC)
      call esmf_ClockGetAlarm(clock, alarmName=RECORD_ALARM, alarm=alarm, _RC)

      do while (currTime < stopTime)
         ! TODO:  include Bill's monitoring log messages here
         call driver%run(phase_idx=GENERIC_RUN_USER, _RC)
         currTime = advance_clock(clock, _RC)

         is_record_time = esmf_AlarmIsRinging(alarm, _RC)
         if (is_record_time) then
            call record(currTime, _RC)
         end if
      end do
      call esmf_TimePrint(currTime, options='string', preString='Cap time after loop: ', _RC)

      _RETURN(_SUCCESS)
   contains

      function advance_clock(clock, rc) result(new_time)
         type(esmf_Time) :: new_time
         type(esmf_Clock), intent(inout) :: clock
         integer, optional, intent(out) :: rc

         call driver%run(phase_idx=GENERIC_RUN_CLOCK_ADVANCE, _RC)
         call driver%clock_advance(_RC)
         call esmf_ClockGet(clock, currTime=new_time, _RC)

         _RETURN(_SUCCESS)
      end function advance_clock

      subroutine record(currTime, rc)
         type(esmf_Time), intent(inout) :: currTime
         integer, optional, intent(out) :: rc

         character(100), allocatable :: iso_time
         character(:), allocatable :: path
         integer :: status
         
         call mapl_PushDirectory(CHECKPOINTS_DIR, _RC)

         call esmf_TimeGet(currTime, timeStringISOFrac=iso_time, _RC)
         path = trim(iso_time)
         call mapl_MakeDirectory(path, _RC)

         call mapl_PushDirectory(path, _RC)
         call driver%write_restart(_RC)
         path = mapl_PopDirectory(_RC) ! checkpoints

         path = mapl_PopDirectory(_RC) ! top
         _RETURN(_SUCCESS)
      end subroutine record

   end subroutine integrate


   subroutine make_directories(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(mapl_AmIRoot())
      call mapl_MakeDirectory(CHECKPOINTS_DIR, force=.true., _RC)
!#      call mapl_MakeDirectory(COLLECTIONS_DIR, force=.true., _RC)
!#      call mapl_MakeDirectory(LOGS_DIR, force=.true., _RC)
      call mapl_barrier()
      _RETURN(_SUCCESS)
   end subroutine make_directories

end module mapl3g_Cap
