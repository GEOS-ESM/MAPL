program overwrite_config_fulltest

   use ESMF

   implicit none

!   interface failed
!      procedure :: failed_integer
!      procedure :: failed_logical
!   end interface failed

!   interface append
!      procedure :: append_character
!   end interface append

   integer, parameter :: MAX_LENGTH = 1024
   integer, parameter :: SUCCESS = ESMF_SUCCESS
   integer, parameter :: FAILURE = SUCCESS - 1
   integer, parameter :: IOSUCCESS = 0
   integer, parameter :: ATTRIB_SIZE = 2
   integer, parameter :: NUM_ATTRIBS = 3
   character(len=*), parameter :: EMPTY_STRING = ''
   character(len=*), parameter :: FMT_ = '(A)'
   character(len=*), parameter :: LABEL1 = 'SwathGrid.Epoch_init:'
!   character(len=*), parameter :: VALUE1 = "'20121113T000000'"
   character(len=*), parameter :: LABEL2 = 'SwathGrid.nc_Time:'
!   character(len=*), parameter :: VALUE2 = "'time'"
   character(len=*), parameter :: LABEL3 = 'SwathGrid.nc_Longitude:'
!   character(len=*), parameter :: VALUE3 = "'cell_across_swath'"
   character(len=*), parameter :: NEW_VALUE = '19991231235959'
!   character(len=*), parameter :: SUCCESS_MESSAGE = 'Success: '
!   character(len=*), parameter :: FAILURE_MESSAGE = 'Failure: '
   character(len=*), parameter :: DELIMITER = ' :: '

   character(len=MAX_LENGTH) :: filename!, message
!   character(len=10) :: date, time
!   integer :: stat 
!   logical :: passed

   call get_command_argument(1, filename)
   write(*, *) 'filename: ' // trim(filename) !// ': ', stat
!   if(stat /= SUCCESS) error stop 'Failed to get filename'
!   if(len_trim(filename) == 0) error stop 'Filename empty'
!   call get_command_argument(1, filename, length, stat)
!   if(stat /= IOSUCCESS .or. length<1) then
!      call date_and_time(date=date, time=time)
!      filename = 'overwrite_' // trim(date) // trim(time) // '_rc.tmp'
!   end if

!   write(*, *) 'temporary file: ' // trim(filename)
   call main(trim(filename))

!   if(passed) then
!      message = SUCCESS_MESSAGE // message
!   else
!      message = FAILURE_MESSAGE // message
!   end if
!
!   write(*, FMT_) 'Result - ' // trim(message)

contains

   subroutine main(filename)
      character(len=*), intent(in) :: filename
      type(ESMF_Config) :: config
      integer :: stat

      call ESMF_Initialize(rc=stat)
      write(*, *) 'initialize: ', stat
!      passed = (stat == SUCCESS)
!      if(.not. passed) message = 'Failed to initialize ESMF'

!      if(passed) then
         call setup(config, filename, rc=stat)
         if(stat == SUCCESS) then
            write(*, *) 'Setup succeeded'
         else
            write(*, *) 'Setup failed'
         end if
!         passed = (stat == SUCCESS)
!         if(.not. passed) message = append('Setup failed', submessage, DELIMITER)
!      end if
      
!      if(passed) then
         call test_overwrite(config, LABEL1, NEW_VALUE, LABEL2, rc=stat)
         if(stat == SUCCESS) then
            write(*, *) 'Test exited normally'
         else
            write(*, *) 'Test exited abnormally'
         end if
!         passed = passed .and. (stat == SUCCESS)
!         if(.not. passed) message = append('test_overwrite failed: ', submessage, DELIMITER)
!      end if

      call ESMF_Finalize(rc=stat)
      write(*, *) 'finalize: ', stat
!      if(stat /= SUCCESS) message = append(message, 'Failed to finalize ESMF', DELIMITER)

   end subroutine main

   subroutine setup(config, filename, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: filename
!      character(len=*), optional, intent(inout) :: message
      integer, intent(out) :: rc
      integer :: stat
      logical :: config_created, is_present!, make_message, 
      logical :: setup_succeeded
      character(len=MAX_LENGTH) :: label!, val

!      make_message = present(message)
      write(*, *) 'SETUP ========================================================================='

      config = ESMF_ConfigCreate(rc=stat)
      write(*, *) 'create: ', stat
      setup_succeeded = (stat == SUCCESS)

!      if(failed(stat, SUCCESS, rc)) return
      config_created = ESMF_ConfigIsCreated(config, rc=stat)
      write(*, *) 'is created: ', stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)
!      if(failed(FAILURE, config_created, rc)) return
!      if(failed(stat, SUCCESS, rc)) return
!      if(make_message) message = 'ESMF_ConfigCreate successful'

!      call write_attributes(build_attributes(), filename, rc=stat)
!      if(failed(stat, SUCCESS, rc=stat)) return
!      if(make_message) message = append(message, 'write_attributes successful', DELIMITER)

      call ESMF_ConfigLoadFile(config, trim(filename), rc=stat)
      write(*, *) 'load ' // trim(filename) // ': ', stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)
!      if(failed(stat, SUCCESS, rc=stat)) return
!      if(make_message) message = append(message, 'ESMF_ConfigLoadFile successful', DELIMITER)
      
      call ESMF_ConfigPrint(config, rc=stat)
      write(*, *) 'present: ', stat

      label = LABEL1
      call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=is_present, rc=stat)
      write(*, *) 'Label 1 ' // trim(label) // ' is present: ', is_present, ': ', stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)

!      call ESMF_ConfigGetAttribute(config, value=val, label=trim(label), rc=stat)
!      if(stat /= SUCCESS) error stop "Cannot get attribute 1: " // trim(label)
!      write(*, fmt='(A, " ", A)') trim(label), trim(val)

      label = LABEL2
      call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=is_present, rc=stat)
      write(*, *) 'Label 2 ' // trim(label) // ' is present: ', is_present, ': ', stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)
!      call ESMF_ConfigGetAttribute(config, value=val, label=trim(label), rc=stat)
!      if(stat /= SUCCESS) error stop "Cannot get attribute 2: " // trim(label)
!      write(*, fmt='(A, " ", A)') trim(label), trim(val)

      label = LABEL3
      call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=is_present, rc=stat)
      write(*, *) 'Label 3 ' // trim(label) // ' is present: ', is_present, ': ', stat
      setup_succeeded = setup_succeeded .and. (stat == SUCCESS)
!      call ESMF_ConfigGetAttribute(config, value=val, label=trim(label), rc=stat)
!      if(stat /= SUCCESS) error stop "Cannot get attribute 3: " // trim(label)
!      write(*, fmt='(A, " ", A)') trim(label), trim(val)

      if(setup_succeeded) then
         rc = SUCCESS
      else
         rc = FAILURE
      end if

      write(*, *) 'END SETUP ====================================================================='

   end subroutine setup

!   function build_attributes() result(attr)
!      character(len=MAX_LENGTH) :: attr(ATTRIB_SIZE, NUM_ATTRIBS)
!
!      attr = EMPTY_STRING
!      attr(1, 1) = LABEL1
!      attr(2, 1) = VALUE1
!      attr(1, 2) = LABEL2
!      attr(2, 2) = VALUE2
!      attr(1, 3) = LABEL3
!      attr(2, 3) = VALUE3
!
!   end function build_attributes

!   subroutine write_attributes(attributes_, filename, rc)
!      character(len=*), intent(in) :: attributes_(:, :)
!      character(len=*), intent(in) :: filename
!      integer, optional, intent(out) :: rc
!      character(len=*), parameter :: DELIMITER = ' ' !achar(9) ! TAB
!      character(len=MAX_LENGTH) :: line
!      integer :: ios, i, iounit
!
!      open(file=trim(filename), newunit=iounit, iostat=ios)
!      if(failed(ios, IOSUCCESS, rc)) then
!         write(*, fmt=FMT_) 'Open failed.'
!      else
!         do i = 1, size(attributes_, 2)
!            line = trim(attributes_(1, i)) // DELIMITER // trim(attributes_(2, i))
!            write(unit=iounit, fmt=FMT_, iostat=ios) trim(line)
!            if(failed(ios, IOSUCCESS, rc, FAILURE)) then
!               write(*, fmt=FMT_) 'Write failed.'
!               exit
!            end if
!         end do
!      end if
!
!      close(unit=iounit, status='KEEP', iostat=ios)
!      if(ios /= IOSUCCESS) write(*, fmt=FMT_) 'Close failed.'
!
!   end subroutine write_attributes

   subroutine test_overwrite(config, label1, value_set, label2, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label1, value_set, label2
      integer, intent(out) :: rc
      logical :: is_present, test_succeeded
      integer :: stat

      write(*, *) 'TEST =========================================================================='
      call ESMF_ConfigSetAttribute(config, value=trim(value_set), label=trim(label1), rc=stat)
      write(*, *) 'Set label1 ' // trim(label1) // ' = ', trim(value_set) // ': ', stat
      test_succeeded = (stat == SUCCESS)
!      test_failed = failed(stat, SUCCESS, rc)
!      if(test_failed) then
!         message = 'Failed to set ' // label1
!      else
         call ESMF_ConfigFindLabel(config, label=trim(label2), isPresent=is_present, rc = stat)
         write(*, *) 'Label2 ' // trim(label2) // ': ', is_present, ': ', stat
         test_succeeded = test_succeeded .and. (stat == SUCCESS)
!         test_failed = failed(stat, SUCCESS, rc)
!      end if

!      if(test_failed) then
!         message = 'Search for ' // label2 // ' failed.'
!      else
!         test_failed = .not. failed(FAILURE, is_present, rc)
!         if(test_failed) message = label2 // ' not found.'
!      end if

!      passed = .not. test_failed

      write(*, *) 'END TEST ===================================================================='

      if(test_succeeded) then
         rc = SUCCESS
      else
         rc = FAILURE
      end if

   end subroutine test_overwrite

!   function failed_integer(stat, check_value, rc, rc_value) result(is_failed)
!      integer, intent(in) :: stat
!      integer, intent(in)  :: check_value
!      integer, optional, intent(inout) :: rc
!      integer, optional, intent(in) :: rc_value
!      logical :: is_failed
!      integer :: rc_value_
!
!      rc_value_ = merge(rc_value, stat, present(rc_value))
!      is_failed = failed(rc_value_, stat == check_value, rc=rc)
!
!   end function failed_integer

!   function failed_logical(stat, check_value, rc) result(is_failed)
!      integer, intent(in) :: stat
!      logical, intent(in)  :: check_value
!      integer, optional, intent(inout) :: rc
!      logical :: is_failed
!
!      is_failed = .not. check_value
!      if(is_failed .and. present(rc)) rc = stat
!
!   end function failed_logical

!   function append_character(message, update, dlmtr) result(appended)
!      character(len=*), intent(in) :: message
!      character(len=*), intent(in) :: update
!      character(len=*), optional, intent(in) :: dlmtr
!      character(len=MAX_LENGTH) :: appended
!
!      if(present(dlmtr)) then
!         appended = trim(message) // dlmtr // update
!      else
!         appended = trim(message) // update
!      end if
!
!   end function append_character

   logical function succeeded(stat, current)
      integer, intent(in) :: stat
      logical, optional, intent(in) :: current
      logical :: current_

      if(present(current)) then
         current_ = current
      else
         current_ = .TRUE.
      endif

      succeeded = current_ .and. (stat == SUCCESS)

   end function succeeded
   
   integer function make_stat(lval)
      logical, intent(in) :: lval

      if(lval) then
         make_stat = SUCCESS
      else
         make_stat = FAILURE
      end if
   end function make_stat

end program overwrite_config_fulltest
