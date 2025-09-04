#include "MAPL.h"

module mapl3g_DriverCap
   use mapl3
   use mapl3g_CapGridComp, only: cap_setservices => setServices
   use mapl_TimeStringConversion, only: string_to_esmf_time
   use mapl_os
   use pflogger
!#   use esmf
   implicit none(type,external)
   private

   public :: mapl_run_driver

   character(*), parameter :: LAST_CHECKPOINT = 'last'
   character(*), parameter :: RECURRING_ALARM_TYPE = 'recurring'
   character(*), parameter :: RING_ONCE_ALARM_TYPE = 'once'

   type CheckpointOptions
      logical :: is_enabled = .false.
      logical :: do_final = .false.
      character(:), allocatable :: path
   end type CheckpointOptions

   type CapOptions
      character(:), allocatable :: name
      character(:), allocatable :: cap_gridcomp_name
      logical :: is_model_pet = .false.
      class(Logger), pointer :: lgr
      type(CheckpointOptions), allocatable :: checkpointing
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
      type(esmf_Clock) :: clock
      type(CapOptions) :: options
      integer :: status

      options = make_cap_options(hconfig, is_model_pet, _RC)
      clock = make_clock(hconfig, options%lgr, _RC)
      driver = make_driver(clock, hconfig, options, _RC)

      _RETURN_UNLESS(is_model_pet)

      ! TODO `initialize_phases` should be a MAPL procedure (name)
      call mapl_DriverInitializePhases(driver, phases=GENERIC_INIT_PHASE_SEQUENCE, _RC)
      call integrate(driver, hconfig, options%checkpointing, _RC)
      call driver%finalize(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine mapl_run_driver

   subroutine integrate(driver, hconfig, checkpointing, rc)
      type(GriddedComponentDriver), intent(inout) :: driver
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CheckpointOptions), intent(in) :: checkpointing
      integer, optional, intent(out) :: rc

      type(esmf_Clock) :: clock
      type(esmf_Time) :: currTime, stopTime
      integer :: status
      character(ESMF_MAXSTR) :: iso_time
      type(ESMF_Time), allocatable :: time_vector(:)
      logical :: has_time_vec, do_run

      clock = driver%get_clock()
      call esmf_ClockGet(clock, currTime=currTime, stopTime=stopTime, _RC)

      has_time_vec = ESMF_HConfigIsDefined(HConfig, keyString='run_times', _RC)
      if (has_time_vec) then
         call fill_time_vector(hconfig, time_vector, _RC)
      else
         allocate(time_vector(0), _STAT)
      end if

      time: do while (currTime < stopTime)

         do_run = time_in_vector(currTime, time_vector)

         if (do_run) then
            call driver%run(phase_idx=GENERIC_RUN_USER, _RC)
         end if
         currTime = advance_clock(driver, _RC)
         if (do_run) then
            call checkpoint(driver, checkpointing, final=.false., _RC)
         end if

      end do time
      call checkpoint(driver, checkpointing, final=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine integrate

   subroutine fill_time_vector(hconfig, time_vector, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(ESMF_Time), intent(inout), allocatable :: time_vector(:)
      integer, optional, intent(out) :: rc

      integer :: status, num_times, i
      character(len=:), allocatable :: temp_str(:)

      temp_str = ESMF_HConfigAsStringSeq(hconfig, stringLen=25, keyString='run_times', _RC)
      num_times = size(temp_str)
      allocate(time_vector(num_times), _STAT)
      do i=1,num_times
         time_vector(i) = string_to_esmf_time(temp_str(i), _RC)
      enddo
      _RETURN(_SUCCESS)
   end subroutine

   function time_in_vector(target_time, time_vector) result(in_vector)
      logical :: in_vector
      type(ESMF_Time), intent(in) :: target_time
      type(ESMF_Time), intent(in) :: time_vector(:)

      integer :: left, right, mid

      in_vector = .false.
      if (size(time_vector) == 0) then
         in_vector = .true.
         return
      end if 

      left = 1
      right = size(time_vector)
      do while (left <= right)
         mid = left + (right - left) / 2
         if (time_vector(mid) == target_time) then
            in_vector = .true.
            return
         else if (time_vector(mid) < target_time) then
            left = mid + 1
         else
            right = mid -1
         end if
      enddo 
   end function time_in_vector

   function advance_clock(driver, rc) result(new_time)
      type(esmf_Time) :: new_time
      type(GriddedComponentDriver), intent(inout) :: driver
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Clock) :: clock

      call driver%run(phase_idx=GENERIC_RUN_CLOCK_ADVANCE, _RC)
      call driver%clock_advance(_RC)

      clock = driver%get_clock()
      call esmf_ClockGet(clock, currTime=new_time, _RC)
      
      _RETURN(_SUCCESS)
   end function advance_clock
   
   subroutine checkpoint(driver, checkpointing, final, rc)
      type(GriddedComponentDriver), intent(inout) :: driver
      type(CheckpointOptions), intent(in) :: checkpointing
      logical, intent(in) :: final
      integer, optional, intent(out) :: rc

      type(esmf_Clock) :: clock
      integer :: alarmCount
      character(:), allocatable :: timestamp
      logical :: is_record_time
      integer :: status

      _RETURN_UNLESS(checkpointing%is_enabled)

      clock = driver%get_clock()
      call esmf_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_RINGING, alarmCount=alarmCount, _RC)
      is_record_time = (alarmCount > 0)

      _RETURN_UNLESS(is_record_time .neqv. final)

      timestamp = get_timestamp(clock, _RC)
      call make_directory(MAPL_PathJoin(checkpointing%path, timestamp), force=.true., _RC)

      ! To avoid inconsistent state under failures, we delete symlink
      ! "last" before writing new checkpoints. Then create new symlink
      call remove_symlink(checkpointing%path, _RC)
      call driver%write_restart(_RC)
      call make_symlink(checkpointing%path, timestamp, _RC)

      _RETURN(_SUCCESS)
   end subroutine checkpoint

   function get_timestamp(clock, rc) result(path)
      character(:), allocatable :: path
      type(esmf_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      character(ESMF_MAXSTR) :: iso_time
      type(ESMF_Time) :: currTime
      integer :: status
      
      call esmf_ClockGet(clock, currTime=currTime, _RC)
      call esmf_TimeGet(currTime, timeStringISOFrac=iso_time, _RC)
      path = trim(iso_time)
      
      _RETURN(_SUCCESS)
   end function get_timestamp

   function make_driver(clock, hconfig, options, rc) result(driver)
      use mapl3g_GenericGridComp, only: generic_SetServices => setServices
      type(GriddedComponentDriver) :: driver
      type(esmf_HConfig), intent(in) :: hconfig
      type(esmf_Clock), intent(in) :: clock
      type(CapOptions), intent(in) :: options
      integer, optional, intent(out) :: rc

      type(esmf_GridComp) :: cap_gridcomp
      integer :: status, user_status
      integer, allocatable :: petList(:)
      
      petList = get_model_pets(options%is_model_pet, _RC)

      cap_gridcomp = mapl_GridCompCreate(options%name, user_setservices(cap_setservices), hconfig, petList=petList, _RC)
      call esmf_GridCompSetServices(cap_gridcomp, generic_setServices, _USERRC)

      driver = GriddedComponentDriver(cap_gridcomp, clock=clock)

      _RETURN(_SUCCESS)
   end function make_driver

   function make_cap_options(hconfig, is_model_pet, rc) result(options)
      type(CapOptions) :: options
      type(esmf_HConfig), intent(in) :: hconfig
      logical, intent(in) :: is_model_pet
      integer, optional, intent(out) :: rc

      integer :: status

      options%name = esmf_HConfigAsString(hconfig, keystring='name', _RC)
      options%is_model_pet = is_model_pet
      options%lgr => logging%get_logger(options%name, _RC)

      options%checkpointing = make_checkpointing_options(hconfig, _RC)
      
      _RETURN(_SUCCESS)
   contains

      function make_checkpointing_options(hconfig, rc) result(options)
         type(CheckpointOptions) :: options
         type(esmf_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(esmf_HConfig) :: checkpointing_cfg
         logical :: has_checkpointing, has_enabled, has_final
         
         has_checkpointing = esmf_HConfigIsDefined(hconfig, keystring='checkpointing', _RC)
         _RETURN_UNLESS(has_checkpointing)

         checkpointing_cfg = esmf_HConfigCreateAt(hconfig, keystring='checkpointing', _RC)
         call get_optional(checkpointing_cfg, keystring='path', value=options%path, _RC)
         has_enabled = esmf_HConfigIsDefined(checkpointing_cfg, keystring='enabled', _RC)
         if (has_enabled) then
            options%is_enabled = esmf_HConfigAsLogical(checkpointing_cfg, keystring='enabled', _RC)

            has_final = esmf_HConfigIsDefined(checkpointing_cfg, keystring='final', _RC)
            if (has_final) then
               options%do_final = esmf_HConfigAsLogical(checkpointing_cfg, keystring='final', _RC)
            end if
         end if
         call esmf_HConfigDestroy(checkpointing_cfg, _RC)

         _RETURN(_SUCCESS)
      end function make_checkpointing_options


      subroutine get_optional(hconfig, keyString, value, rc)
         type(esmf_HConfig), intent(in) :: hconfig
         character(*), intent(in) :: keystring
         character(:), allocatable, intent(inout) :: value
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_keyString

         has_keyString = esmf_HConfigIsDefined(hconfig, keystring=keystring, _RC)
         _RETURN_UNLESS(has_keyString)

          value = esmf_HConfigAsString(hconfig, keystring=keystring, _RC)
         _RETURN(_SUCCESS)
      end subroutine get_optional

   end function make_cap_options

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

   function make_clock(hconfig, lgr, rc) result(clock)
      type(esmf_Clock) :: clock
      type(esmf_HConfig), intent(in) :: hconfig
      class(Logger), intent(inout) :: lgr
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Alarm) :: record_alarm
      type(esmf_HConfig) :: clock_cfg, restart_cfg
      type(ESMF_Time) :: startTime, stopTime, currTime
      type(ESMF_Time) :: end_of_segment
      type(ESMF_TimeInterval) :: timeStep, segment_duration
      type(ESMF_TimeInterval), allocatable :: repeatDuration
      logical :: has_repeatDuration
      character(:), allocatable :: cap_restart_file
      character(ESMF_MAXSTR) :: iso_time

      cap_restart_file = esmf_HConfigAsString(hconfig, keyString='restart', _RC)
      restart_cfg = esmf_HConfigCreate(filename=cap_restart_file, _RC)
      currTime = mapl_HConfigAsTime(restart_cfg, keystring='currTime', _RC)
      iso_time = esmf_HConfigAsString(restart_cfg, keystring='currTime', _RC)
      call lgr%info('current time: %a', trim(iso_time)) 
      call esmf_HConfigDestroy(restart_cfg, _RC)

      clock_cfg = esmf_HConfigCreateAt(hconfig, keystring='clock', _RC)
      
      startTime = mapl_HConfigAsTime(clock_cfg, keystring='start', _RC)
      call esmf_TimeGet(startTime, timeStringISOFrac=iso_time, _RC)
      call lgr%info('start time: %a', trim(iso_time)) 

      stopTime = mapl_HConfigAsTime(clock_cfg, keystring='stop', _RC)
      call esmf_TimeGet(stopTime, timeStringISOFrac=iso_time, _RC)
      call lgr%info('stop time: %a', trim(iso_time)) 

      timeStep = mapl_HConfigAsTimeInterval(clock_cfg, keystring='dt', _RC)
      call esmf_TimeGet(stopTime, timeStringISOFrac=iso_time, _RC)
      call lgr%info('time step: %a', trim(iso_time)) 

      segment_duration = mapl_HConfigAsTimeInterval(clock_cfg, keystring='segment_duration', _RC)
      end_of_segment = currTime + segment_duration
      call esmf_TimeGet(end_of_segment, timeStringISOFrac=iso_time, _RC)
      call lgr%info('segment stop time: %a', trim(iso_time))

      has_repeatDuration = esmf_HConfigIsDefined(clock_cfg, keystring='repeat_duration', _RC)
      if (has_repeatDuration) then
         allocate(repeatDuration) ! anticipating NAG compiler issue here
         repeatDuration = mapl_HConfigAsTimeInterval(clock_cfg, keystring='repeat_duration', _RC)
         call esmf_TimeIntervalGet(repeatDuration, timeStringISOFrac=iso_time, _RC)
         call lgr%info('repeat duration: %a', trim(iso_time))
      end if
      
      clock = esmf_ClockCreate(timeStep=timeStep, &
           startTime=startTime, stopTime=end_of_segment, &
           refTime=startTime, &
           repeatDuration=repeatDuration, _RC)
      call ESMF_ClockSet(clock, currTime=currTime, _RC)

      call esmf_HConfigDestroy(clock_cfg, _RC)

      _RETURN(_SUCCESS)
   end function make_clock

   subroutine add_record_alarms(clock, hconfig, rc)
      type(esmf_Clock), intent(inout) :: clock
      type(esmf_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      type(esmf_HConfig) :: alarms_cfg, alarm_cfg, checkpointing_cfg
      logical :: has_alarms, has_checkpointing
      integer :: i, num_alarms
      integer :: status

      has_checkpointing = esmf_HConfigIsDefined(hconfig, keystring='checkpointing', _RC)
      _RETURN_UNLESS(has_checkpointing)
      checkpointing_cfg = esmf_HConfigCreateAt(hconfig, keystring='checkpointing', _RC)

      has_alarms = esmf_HConfigIsDefined(checkpointing_cfg, keystring='alarms', _RC)
      if (has_alarms) then
         alarms_cfg = esmf_HConfigCreateAt(checkpointing_cfg, keystring='alarms', _RC)
         num_alarms = esmf_HConfigGetSize(alarms_cfg, _RC)
         do i = 1, num_alarms
            alarm_cfg = esmf_HConfigCreateAt(alarms_cfg, index=i, _RC)
            call add_alarm(clock, alarm_cfg, _RC)
            call esmf_HConfigDestroy(alarm_cfg, _RC)
         end do
      end if

      call esmf_HConfigDestroy(alarms_cfg, _RC)
      call esmf_HConfigDestroy(checkpointing_cfg, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine add_alarm(clock, cfg, rc)
         type(esmf_Clock), intent(inout) :: clock
         type(esmf_HConfig), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         integer :: status
         type(esmf_Alarm) :: alarm
         character(:), allocatable :: alarm_type

         alarm_type = get_alarm_type(cfg, _RC)
         select case (alarm_type)
         case (RECURRING_ALARM_TYPE)
            call add_recurring_alarm(clock, cfg, _RC)
         case (RING_ONCE_ALARM_TYPE)
            call add_ring_once_alarms(clock, cfg, _RC)
         case default
            _FAIL('unknown alarm type: ' // alarm_type)
         end select

         _RETURN(_SUCCESS)
      end subroutine add_alarm

      subroutine add_recurring_alarm(clock, cfg, rc)
         type(esmf_Clock), intent(inout) :: clock
         type(esmf_HConfig), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         type(esmf_Alarm) :: alarm
         type(esmf_TimeInterval) :: ringInterval
         type(esmf_Time) :: refTime, currTime
         logical :: has_reftime
         integer :: status

         ringInterval = mapl_HConfigAsTimeInterval(cfg, keystring='frequency', _RC)
         has_refTime = esmf_HConfigIsDefined(cfg, keystring='refTime', _RC)
         if (has_refTime) then
            refTime = mapl_HConfigAsTime(cfg, keystring='refTime', _RC)
         else
            call esmf_ClockGet(clock, currTime=currTime, _RC)
            refTime = currTime
         end if
         refTime = mapl_HConfigAsTime(cfg, keystring='refTime', _RC)

         alarm = esmf_AlarmCreate(clock, ringTime=refTime, ringInterval=ringInterval, sticky=.false., _RC)
         call esmf_AlarmRingerOff(alarm, _RC)

         _RETURN(_SUCCESS)
      end subroutine add_recurring_alarm

      subroutine add_ring_once_alarms(clock, cfg, rc)
         type(esmf_Clock), intent(inout) :: clock
         type(esmf_HConfig), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         type(ESMF_HConfig) :: subcfg
         type(esmf_Alarm) :: alarm
         type(esmf_Time) :: ringTime, currTime
         type(esmf_TimeInterval) :: offset
         integer :: i, num_items
         logical :: has_offsets, has_times
         integer :: status
         character(:), allocatable :: iso_string

         has_times = esmf_HConfigIsDefined(cfg, keystring='times', _RC)
         has_offsets = esmf_HConfigIsDefined(cfg, keystring='offsets', _RC)
         _ASSERT(has_times .neqv. has_offsets, 'alarm list have either times or offsets but not both')

         if (has_times) then
            subcfg = esmf_HConfigCreateAt(cfg, keystring='times', _RC)
         elseif (has_offsets) then
            call esmf_ClockGet(clock, currTime=currTime, _RC)
            subcfg = esmf_HConfigCreateAt(cfg, keystring='offsets', _RC)
         else
            _FAIL('alarm type is not supported')
         end if

         num_items = esmf_HConfigGetSize(subcfg, _RC)
         
         do i = 1, num_items
            iso_string = esmf_HConfigAsString(subcfg,  index=i, _RC)
            if (has_times) then
               call esmf_TimeSet(ringTime, timeString=iso_string, _RC)
            else if (has_offsets) then
               call esmf_TimeIntervalSet(offset, timeIntervalString=iso_string, _RC)
               ringTime = currTime + offset
            end if
            alarm = esmf_AlarmCreate(clock, ringTime=ringTime, sticky=.false., _RC)
            call esmf_AlarmRingerOff(alarm, _RC)
         end do

         call esmf_HConfigDestroy(subcfg, _RC)

         _RETURN(_SUCCESS)
      end subroutine add_ring_once_alarms

      function get_alarm_type(cfg, rc) result(alarm_type)
         character(:), allocatable :: alarm_type
         type(esmf_HConfig), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_frequency, has_times, has_offsets

         alarm_type = 'unknown'

         has_frequency = esmf_HConfigIsDefined(cfg, keystring='frequency', _RC)
         if (has_frequency) then
            alarm_type = RECURRING_ALARM_TYPE
            _RETURN(_SUCCESS)
         end if

         has_times = esmf_HConfigIsDefined(cfg, keystring='times', _RC)
         has_offsets = esmf_HConfigIsDefined(cfg, keystring='offsets', _RC)
         if (has_times .or. has_offsets) then
            alarm_type = RING_ONCE_ALARM_TYPE
            _RETURN(_SUCCESS)
         end if

         _RETURN(_SUCCESS)
      end function get_alarm_type

   end subroutine add_record_alarms

   ! Only make the directory on root process.
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

   subroutine remove_symlink(checkpointing_path, rc)
      character(*), intent(in) :: checkpointing_path
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: path
      logical :: last_exists
      integer :: status

      path = MAPL_PathJoin(checkpointing_path, LAST_CHECKPOINT)
      last_exists = MAPL_DirectoryExists(path, _RC)
      if (last_exists) then
         if (MAPL_AmIRoot()) then
            call MAPL_RemoveFile(path, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine remove_symlink

   subroutine make_symlink(checkpointing_path, target_name, rc)
      character(*), intent(in) :: checkpointing_path
      character(*), intent(in) :: target_name
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: path
      integer :: status

      if (MAPL_AmIRoot()) then
         call MAPL_PushDirectory(checkpointing_path, _RC)
         call MAPL_MakeSymbolicLink(src_path=target_name, link_path=LAST_CHECKPOINT, is_directory=.true., _RC)
         path = MAPL_PopDirectory(_RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine make_symlink

end module mapl3g_DriverCap
