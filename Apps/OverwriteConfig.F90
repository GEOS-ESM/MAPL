program overwrite_config

   use, intrinsic :: iso_fortran_env, only: R64 => real64
   use ESMF

   implicit none

   interface failed
      procedure :: failed_integer
      procedure :: failed_logical
   end interface failed

   interface append
      procedure :: append_character
   end interface append

   integer, parameter :: MAX_LENGTH = 1024
   integer, parameter :: SUCCESS = ESMF_SUCCESS
   integer, parameter :: FAILURE = SUCCESS - 1
   integer, parameter :: IOSUCCESS = 0
   integer, parameter :: ATTRIB_SIZE = 2
   integer, parameter :: NUM_ATTRIBS = 3
   character(len=*), parameter :: EMPTY_STRING = ''
   character(len=*), parameter :: FMT_ = '(A)'
   character(len=*), parameter :: LABEL1 = 'SwathGrid.Epoch_init:'
   character(len=*), parameter :: VALUE1 = "'20121113T000000'"
   character(len=*), parameter :: LABEL2 = 'SwathGrid.nc_Time:'
   character(len=*), parameter :: VALUE2 = "'time'"
   character(len=*), parameter :: LABEL3 = 'SwathGrid.nc_Longitude:'
   character(len=*), parameter :: VALUE3 = "'cell_across_swath'"
   character(len=*), parameter :: SUCCESS_MESSAGE = 'Success: '
   character(len=*), parameter :: FAILURE_MESSAGE = 'Failure: '
   character(len=*), parameter :: DELIMITER = ' :: '

   character(len=MAX_LENGTH) :: filename, message
   character(len=10) :: date, time
   integer :: stat, length
   logical :: passed

   call get_command_argument(1, filename, length, stat)
   if(stat /= 0 .or. length<1) then
      call date_and_time(date=date, time=time)
      filename = 'overwrite_' // trim(date) // trim(time) // '_rc.tmp'
   end if

   write(*, *) 'temporary file: ' // trim(filename)
   call main(trim(filename), message, passed)

   if(passed) then
      message = SUCCESS_MESSAGE // message
   else
      message = FAILURE_MESSAGE // message
   end if

   write(*, FMT_) 'Result - ' // trim(message)

contains

   subroutine main(filename, message, passed)
      character(len=*), intent(in) :: filename
      character(len=MAX_LENGTH), intent(out) :: message
      character(len=MAX_LENGTH) :: submessage
      logical, intent(out) :: passed
      type(ESMF_Config) :: config
      integer :: stat, iounit, ios

      call ESMF_Initialize(rc=stat)
      passed = (stat == SUCCESS)
      if(.not. passed) message = 'Failed to initialize ESMF'

      if(passed) then
         open(file=trim(filename), newunit=iounit, iostat=ios)
         passed = (ios == IOSUCCESS)
         if(.not. passed) message = 'Failed to open tempoarary file: ' // filename
      end if

      if(passed) then
         call setup(config, filename, iounit, message=submessage, rc=stat)
         passed = (stat == SUCCESS)
         message = 'Setup failed'
         if(.not. passed) message = append(message, submessage, DELIMITER)
      end if
      
      if(passed) then
         call test_overwrite(config, LABEL1, VALUE1, LABEL2, passed, message = submessage, rc=stat)
         passed = passed .and. (stat == SUCCESS)
         if(.not. passed) message = append('test_overwrite failed: ', submessage, DELIMITER)
      end if

      if(passed) call ESMF_Finalize(rc=stat)
      
      close(iounit, status='DELETE', iostat=ios)
      if(stat /= SUCCESS) write(*, *) 'Error closing ' // trim(filename) // '. Manually delete if necessary.'

   end subroutine main

   subroutine setup(config, filename, iounit, message, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: filename
      integer, intent(in) :: iounit
      character(len=*), optional, intent(inout) :: message
      integer, optional, intent(out) :: rc
      integer :: stat 
      logical :: config_created, make_message

      make_message = present(message)

      config = ESMF_ConfigCreate(rc=stat)
      if(failed(stat, SUCCESS, rc)) return

      config_created = ESMF_ConfigIsCreated(config, rc=stat)
      if(failed(FAILURE, config_created, rc)) return
      if(failed(stat, SUCCESS, rc)) return

      if(make_message) message = 'ESMF_ConfigCreate successful'

      call write_attributes(build_attributes(), iounit, rc=stat)
      if(failed(stat, SUCCESS, rc=stat)) return

      if(make_message) message = append(message, 'write_attributes successful', DELIMITER)
      call ESMF_ConfigLoadFile(config, trim(filename), rc=stat)
      if(failed(stat, SUCCESS, rc=stat)) return

      if(make_message) message = append(message, 'ESMF_ConfigLoadFile successful', DELIMITER)

   end subroutine setup

   function build_attributes() result(attr)
      character(len=MAX_LENGTH) :: attr(ATTRIB_SIZE, NUM_ATTRIBS)

      attr = EMPTY_STRING
      attr(1, 1) = LABEL1
      attr(2, 1) = VALUE1
      attr(1, 2) = LABEL2
      attr(2, 2) = VALUE2
      attr(1, 3) = LABEL3
      attr(2, 3) = VALUE3

   end function build_attributes

   subroutine write_attributes(attributes_, iounit, rc)
      character(len=*), intent(in) :: attributes_(:, :)
      integer, intent(in) :: iounit
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: DELIMITER = achar(9) ! TAB
      character(len=MAX_LENGTH) :: line
      logical :: is_open
      integer :: ios, i

      inquire(unit=iounit, opened=is_open, iostat=ios)
      if(failed(ios, IOSUCCESS, rc, FAILURE)) return
      if(failed(FAILURE, is_open, rc)) return

      do i = 1, size(attributes_, 2)
         line = trim(attributes_(1, i)) // DELIMITER // trim(attributes_(2, i))
         write(unit=iounit, fmt=FMT_, iostat=ios) trim(line)
         if(failed(ios, IOSUCCESS, rc, FAILURE)) exit
      end do

   end subroutine write_attributes

   subroutine test_overwrite(config, label_set, value_set, label_find, passed, message, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label_set, value_set, label_find
      logical, intent(out) :: passed
      character(len=MAX_LENGTH), intent(out) :: message
      integer, optional, intent(out) :: rc
      logical :: is_present, test_failed

      call ESMF_ConfigSetAttribute(config, value=value_set, label=label_set, rc=stat)
      test_failed = failed(stat, SUCCESS, rc)
      if(test_failed) then
         message = 'Failed to set ' // label_set
      else
         call ESMF_ConfigFindLabel(config, label=label_find, isPresent=is_present, rc = stat)
         test_failed = failed(stat, SUCCESS, rc)
      end if

      if(test_failed) then
         message = 'Search for ' // label_find // ' failed.'
      else
         test_failed = .not. failed(FAILURE, is_present, rc)
      end if
      
      if(test_failed) message = label_find // ' not found.'

      passed = .not. test_failed

   end subroutine test_overwrite

   function failed_integer(stat, check_value, rc, rc_value) result(is_failed)
      integer, intent(in) :: stat
      integer, intent(in)  :: check_value
      integer, optional, intent(inout) :: rc
      integer, optional, intent(in) :: rc_value
      logical :: is_failed
      integer :: rc_value_

      rc_value_ = merge(rc_value, stat, present(rc_value))
      is_failed = failed(rc_value_, stat == check_value, rc=rc)

   end function failed_integer

   function failed_logical(stat, check_value, rc) result(is_failed)
      integer, intent(in) :: stat
      logical, intent(in)  :: check_value
      integer, optional, intent(inout) :: rc
      logical :: is_failed

      is_failed = .not. check_value
      if(is_failed .and. present(rc)) rc = stat

   end function failed_logical

   function append_character(message, update, dlmtr) result(appended)
      character(len=*), intent(in) :: message
      character(len=*), intent(in) :: update
      character(len=*), optional, intent(in) :: dlmtr
      character(len=MAX_LENGTH) :: appended
      character(len=:), allocatable :: dlmtr_      

      if(present(dlmtr)) then
         appended = trim(message) // dlmtr_ // update
      else
         appended = trim(message) // update
      end if

   end function append_character

end program overwrite_config
