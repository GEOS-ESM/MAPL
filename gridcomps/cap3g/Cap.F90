#include "MAPL.h"

module mapl3g_Cap
   use mapl3
   use mapl3g_CapGridComp, only: cap_setservices => setServices
   use mapl_TimeStringConversion, only: hconfig_to_esmf_timeinterval
   use mapl_os
!#   use esmf
   implicit none(type,external)
   private

   public :: mapl_run_driver

   character(*), parameter :: RECORD_ALARM_NAME = 'record'
   character(*), parameter :: CHECKPOINTS_DIR = 'checkpoints'
   character(*), parameter :: COLLECTIONS_DIR = 'collections'
   character(*), parameter :: LOGS_DIR = 'logs'
   character(*), parameter :: LAST_CHECKPOINT = 'last'

   type CapOptions
      character(:), allocatable :: name
      type(esmf_Time) :: startTime
      type(esmf_Time) :: stopTime
      type(esmf_TimeInterval) :: timeStep
      type(esmf_HConfig) :: cap_gc_hconfig

      logical :: checkpointing = .true.
      type(esmf_Time) :: record_ringtime
      type(esmf_TimeInterval), allocatable :: record_frequency
      logical :: record_enabled = .false.
   end type CapOptions

contains


   subroutine mapl_run_driver(hconfig, is_model_pet, unusable, servers, rc)
      USE mapl_ApplicationSupport
      type(esmf_HConfig), intent(inout) :: hconfig
      logical, intent(in) :: is_model_pet
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(esmf_GridComp), optional, intent(in) :: servers(:)
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver) :: driver
      type(CapOptions) :: options
      integer :: status

      options = get_driver_options(hconfig, _RC)
      driver = make_driver(options, is_model_pet, _RC)
      _RETURN_UNLESS(is_model_pet)

      call make_directories(_RC)
      call initialize_phases(driver, phases=GENERIC_INIT_PHASE_SEQUENCE, _RC)
      call integrate(driver, options%record_enabled, _RC)
      call driver%finalize(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_run_driver

   subroutine integrate(driver, checkpointing, rc)
      type(GriddedComponentDriver), intent(inout) :: driver
      logical, intent(in) :: checkpointing
      integer, optional, intent(out) :: rc

      type(esmf_Clock) :: clock
      type(esmf_Time) :: currTime, stopTime
      type(esmf_Alarm) :: alarm
      integer :: status

      clock = driver%get_clock()
      call esmf_ClockGet(clock, currTime=currTime, stopTime=stopTime, _RC)
      call esmf_ClockGetAlarm(clock, alarmName=RECORD_ALARM_NAME, alarm=alarm, _RC)

      time: do while (currTime < stopTime)
         ! TODO:  include Bill's monitoring log messages here
         call driver%run(phase_idx=GENERIC_RUN_USER, _RC)
         currTime = advance_clock(driver, clock, _RC)

         if (checkpointing) then
            call checkpoint(driver, final=.false., _RC)
         end if

      end do time

      call esmf_TimePrint(currTime, options='string', preString='Cap time after loop: ', _RC)

      if (checkpointing) then
         call checkpoint(driver, final=.true., _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine integrate

   function advance_clock(driver, clock, rc) result(new_time)
      type(esmf_Time) :: new_time
      type(GriddedComponentDriver), intent(inout) :: driver
      type(esmf_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      call driver%run(phase_idx=GENERIC_RUN_CLOCK_ADVANCE, _RC)
      call driver%clock_advance(_RC)
      call esmf_ClockGet(clock, currTime=new_time, _RC)
      
      _RETURN(_SUCCESS)
   end function advance_clock
   
   subroutine checkpoint(driver, final, rc)
      type(GriddedComponentDriver), intent(inout) :: driver
      logical, intent(in) :: final
      integer, optional, intent(out) :: rc

      type(esmf_Clock) :: clock
      type(esmf_Time) :: currTime
      type(esmf_Alarm) :: alarm
      character(100), allocatable :: iso_time
      character(:), allocatable :: path
      logical :: is_record_time
      logical :: last_exists
      integer :: status

      clock = driver%get_clock()
      call esmf_ClockGetAlarm(clock, alarmName=RECORD_ALARM_NAME, alarm=alarm, _RC)
      
      is_record_time = esmf_AlarmIsRinging(alarm, _RC)
      _RETURN_UNLESS(is_record_time .neqv. final)
      
      call mapl_PushDirectory(CHECKPOINTS_DIR, _RC)
      
      call esmf_ClockGet(clock, currTime=currTime, _RC)
      call esmf_TimeGet(currTime, timeStringISOFrac=iso_time, _RC)
      path = trim(iso_time)
      call make_directory(path, _RC)
      call mapl_PushDirectory(path, _RC)

      call driver%write_restart(_RC)

      path = mapl_PopDirectory(_RC) ! checkpoints

      if (mapl_AmIRoot()) then
         inquire(file=LAST_CHECKPOINT, exist=last_exists) ! assumes LAST_CHECKPOINT is symlink
         if (last_exists) then
            call mapl_RemoveFile(LAST_CHECKPOINT, _RC)
         end if
         call mapl_MakeSymbolicLink(src_path=path, link_path=LAST_CHECKPOINT, is_directory=.true., _RC)
      end if
      
      path = mapl_PopDirectory(_RC) ! top
      _RETURN(_SUCCESS)
   end subroutine checkpoint

   function make_driver(options, is_model_pet, rc) result(driver)
      use mapl3g_GenericGridComp, only: generic_SetServices => setServices
      type(GriddedComponentDriver) :: driver
      type(CapOptions), intent(in) :: options
      logical, intent(in) :: is_model_pet
      integer, optional, intent(out) :: rc

      type(esmf_GridComp) :: cap_gridcomp
      type(esmf_Clock) :: clock
      integer :: status, user_status
      integer, allocatable :: petList(:)
      
      clock = create_clock(options, _RC)
      petList = get_model_pets(is_model_pet, _RC)
      cap_gridcomp = mapl_GridCompCreate(options%name, user_setservices(cap_setservices), options%cap_gc_hconfig, petList=petList, _RC)

      call esmf_GridCompSetServices(cap_gridcomp, generic_setServices, userRC=user_status, _RC)
      _VERIFY(user_status)

      driver = GriddedComponentDriver(cap_gridcomp, clock=clock)

      _RETURN(_SUCCESS)
   end function make_driver

   function get_driver_options(hconfig, rc) result(options)
      type(CapOptions) :: options
      type(esmf_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Time) :: end_of_segment
      type(esmf_TimeInterval) :: timeStep, segment_duration, record_offset
      type(esmf_HConfig) :: clock_config
      logical :: has_record_frequency, has_record_offset, has_checkpointing

      options%name = esmf_HConfigAsString(hconfig, keystring='name', _RC)
      options%cap_gc_hconfig = esmf_HConfigCreateAt(hconfig, keystring='cap_gc', _RC)

      clock_config = esmf_HConfigCreateAt(hconfig, keystring='clock', _RC)

      call set_time(options%startTime, 'start', clock_config, _RC)
      call esmf_TimePrint(options%startTime, options='string', prestring='start time set: ' ,_RC)

      call set_time(options%stopTime, 'stop', clock_config, _RC)
      call esmf_TimePrint(options%stopTime, options='string', prestring='stop time set: ', _RC)

      options%timeStep = hconfig_to_esmf_timeinterval(clock_config, 'dt', _RC)
      segment_duration = hconfig_to_esmf_timeinterval(clock_config, 'segment_duration', _RC)

      end_of_segment = options%startTime + segment_duration
      if (end_of_segment < options%stopTime) options%stopTime = end_of_segment
      call esmf_TimePrint(options%stopTime, options='string', prestring='segment stop time: ', _RC)

      options%record_ringTime = options%stopTime ! default
      has_checkpointing = ESMF_HConfigIsDefined(clock_config, keystring='checkpointing', _RC)
      if (has_checkpointing) then
         options%checkpointing = ESMF_HConfigAsLogical(hconfig, keystring='checkpointing', _RC)

         has_record_frequency = ESMF_HConfigIsDefined(hconfig, keystring='record_frequency', _RC)
         if (has_record_frequency) then
            options%record_enabled = .true.
            options%record_frequency = hconfig_to_esmf_timeinterval(hconfig, 'record_frequency', _RC)
         end if
         has_record_offset = ESMF_HConfigIsDefined(hconfig, keystring='record_offset', _RC)
         if (has_record_offset) then
            record_offset = hconfig_to_esmf_timeinterval(hconfig, 'record_offset', _RC)
            options%record_ringTime = options%startTime + record_offset
         end if
      end if

      _RETURN(_SUCCESS)

   contains

      subroutine set_time(time, key, hconfig, rc)
         type(esmf_Time), intent(out) :: time
         character(*), intent(in) :: key
         type(esmf_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
         
         integer :: status
         character(:), allocatable :: iso_time

         _HERE, key
         iso_time = esmf_HConfigAsString(hconfig, keystring=key, _RC)
         _HERE, 'iso_time: ', iso_time
         call esmf_TimeSet(time, timeString=iso_time, _RC)
         
         _RETURN(_SUCCESS)
      end subroutine set_time
      
   end function get_driver_options

   ! Create function that accepts a logical flag returns list of mpi processes that have .true..
   function get_model_pets(flag, rc) result(petList)
      integer, allocatable :: petList(:)
      logical, intent(in) :: flag
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_VM) :: vm
      type(ESMF_Logical), allocatable, target :: flags(:)
      type(ESMF_Logical), target :: flag_as_array(1)
      integer :: i, petCount

      integer, target :: i1(1)
      integer, target, allocatable :: i2(:)

      call esmf_VMGetCurrent(vm, _RC)
      call esmf_VMGet(vm, petCount=petCount, _RC)
      allocate(flags(petCount))
      flag_as_array = [flag]
      call esmf_VMAllGather(vm, sendData=flag_as_array, recvData=flags, count=1, _RC)
      petList = pack([(i, i=0,petCount-1)], flags==ESMF_TRUE)

      _RETURN(_SUCCESS)
   end function get_model_pets

   function create_clock(options, rc) result(clock)
      type(esmf_Clock) :: clock
      type(CapOptions), intent(in) :: options
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Alarm) :: record_alarm

      clock = esmf_ClockCreate(timeStep=options%timeStep, startTime=options%startTime, stopTime=options%stopTime, &
           refTime=options%startTime, _RC)

      record_alarm = esmf_AlarmCreate(clock, name=RECORD_ALARM_NAME, &
           ringTime=options%record_ringTime, &
           ringInterval=options%record_frequency, &
           enabled=options%record_enabled, &
           sticky=.false., _RC)


      _RETURN(_SUCCESS)

   end function create_clock


   subroutine make_directories(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      call make_directory(CHECKPOINTS_DIR, force=.true., _RC)
!#      call make_directory(COLLECTIONS_DIR, force=.true., _RC)
!#      call make_directory(LOGS_DIR, force=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine make_directories

   subroutine make_directory(path, force, rc)
      character(*), intent(in) :: path
      logical, optional, intent(in) :: force
      integer, optional, intent(out) :: rc

      integer :: status

      if (mapl_AmIRoot()) then
         call mapl_MakeDirectory(path, force=force, _RC)
      end if
      call mapl_Barrier(_RC)

      _RETURN(_SUCCESS)
   end subroutine make_directory
      
end module mapl3g_Cap
