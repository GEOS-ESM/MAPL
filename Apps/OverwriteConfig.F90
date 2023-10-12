program overwrite_config

   use ESMF

   implicit none

   interface failed
      procedure :: failed_integer
      procedure :: failed_logical
   end interface failed

   interface fail_close
      procedure :: fail_integer_close
      procedure :: fail_logical_close
   end interface fail_close

   integer, parameter :: MAX_LENGTH = 1024
   integer, parameter :: SUCCESS = ESMF_SUCCESS
   integer, parameter :: FAILURE = SUCCESS - 1
   integer, parameter :: IOSUCCESS = 0

contains

   subroutine main(rc)
      integer, optional, intent(out) :: rc
      integer, parameter :: NROWS = 2
      integer, parameter :: NCOLS = 3
      character(len=*), parameter :: EMPTY_STRING = ''
      character(len=*), parameter :: LABEL1 = 'SwathGrid.Epoch_init:'
      character(len=*), parameter :: VALUE1 = "'20121113T000000'"
      character(len=*), parameter :: LABEL2 = 'SwathGrid.nc_Time:'
      character(len=*), parameter :: VALUE2 = "'time'"
      character(len=*), parameter :: LABEL3 = 'SwathGrid.nc_Longitude:'
      character(len=*), parameter :: VALUE3 = "'cell_across_swath'"
      integer :: stat, iounit
      type(ESMF_Config) :: config
      character(len=MAX_LENGTH) :: filename
      character(len=MAX_LENGTH) :: message
      logical :: test_passed

      filename = 'temp_file'
      call setup(config, filename, iounit, rc = stat)
      if(fail_close(stat, SUCCESS, rc)) then
         write(*, fmt='(A)') 'Setup failed'
         test_passed = .FALSE.
      end if

      if(test_passed) then
         call test_overwrite(config, LABEL1, VALUE1, LABEL2, test_passed, message, rc=stat)
      end if

      if(fail_close(stat, .not. test_passed, rc)) write(*, fmt='(A)') trim(messaage)

      if(test_passed) then
         if (fail_close(stat, SUCCESS, rc)) write(*, fmt='(A)') 'test_overwrite failed.'
      end if

   contains

      subroutine setup(config, filename, iounit, rc)
         type(ESMF_Config), intent(inout) :: config
         character(len=*), intent(in) :: filename
         integer, intent(inout) :: iounit
         integer, optional, intent(out) :: rc
         integer :: stat, ios
         logical :: config_not_created

         config = ESMF_ConfigCreate()
         if(fail_close(stat, SUCCESS, iounit, rc)) return

         config_not_created = .not. ESMF_ConfigIsCreated(config, rc=stat)
         if(fail_close(stat, config_not_created, iounit, rc)) return

         open(file=filename, newunit=iounit, iostat=ios)
         if(fail_close(ios, IOSUCCESS, rc, FAILURE)) return

         call write_attributes(build_attributes(), iounit, rc=stat)
         if(fail_close(stat, SUCCESS, rc)) return

         call ESMF_ConfigLoadFile(config, trim(filename),  rc=stat)
         if(fail_close(stat, SUCCESS, rc)) return

      end subroutine setup

      function build_attributes(nrows, ncols) result(attr)
         integer, intent(in) :: nr, nc
         character(len=MAX_LENGTH) :: attr(nr, nc)

         attr = EMPTY_STRING
         if((nr == NROWS) .and. (nc == NCOLS)) then
            attr(1, 1) = LABEL1
            attr(2, 1) = VALUE1
            attr(1, 2) = LABEL2
            attr(2, 2) = VALUE2
            attr(1, 3) = LABEL3
            attr(2, 3) = VALUE3
         end if

      end function build_attributes

   end subroutine main

   subroutine write_attributes(attributes_, iounit, rc)
      character(len=*), intent(in) :: attributes_(:, :)
      integer, intent(in) :: iounit
      integer, optional, intent(out) :: rc
      character, parameter :: DELIMITER = ' '
      character(len=MAX_LENGTH) :: line
      logical :: is_open
      integer :: ios, i

      inquire(unit=iounit, opened=is_open, iostat=ios)
      if(failed(ios, IOSUCCESS, rc, FAILURE)) return
      if(failed(ios, .not. open, rc, FAILURE)) return

      do i = 1, size(attributes_, 2)
         line = trim(attributes_(1, i)) // DELIMITER // trim(attributes_(2, i))
         write(unit=iounit, fmt='(A)', iostat=ios) trim(line)
         if(failed(ios, IOSUCCESS, rc, FAILURE)) exit
      end do

   end subroutine write_attributes

   subroutine test_overwrite(config, label_set, value_set, label_find, passed, message, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label_set, value_set, label_find
      logical, intent(out) :: passed
      character(len=MAX_LENGTH), intent(out) :: message
      integer, optional, intent(out) :: rc
      logical :: is_present

      call ESMF_ConfigSetAttribute(config, value=value_set, label=label_set, rc=stat)
      passed = .not. failed(stat, SUCCESS, rc)
      if(.not. passed) message = 'Failed to set ' // label_set

      if(passed) then
         call ESMF_ConfigFindLabel(config, label=label_find, isPresent=is_present, rc = stat)
      end if

      passed = .not. failed(stat, SUCCESS, rc)
      if(.not. passed) message = 'Search for ' // label_find // ' failed.'
      if(passed) passed = .not. failed(stat, is_present, rc)
      if(.not. passed) message = label_find // ' not found.'

   end subroutine test_overwrite

   function failed_integer(stat, check_value, rc, rc_value) result(lreturn)
      integer, intent(in) :: stat
      integer, intent(in)  :: check_value
      integer, optional, intent(inout) :: rc
      integer, optional, intent(in) :: rc_value
      logical :: lreturn

      lreturn = failed(stat, stat /= check_value, rc=rc, rc_value)

   end function failed_integer

   function failed_logical(stat, check_value, rc, rc_value) result(lreturn)
      integer, intent(in) :: stat
      logical, intent(in)  :: check_value
      integer, optional, intent(inout) :: rc
      integer, optional, intent(in) :: rc_value
      logical :: lreturn

      lreturn = check_value
      if(lreturn .and. present(rc)) rc = merge(rc_value, stat, present(rc_value))

   end function failed_logical

   function fail_integer_close(stat, check_value, iounit, rc, rc_value) result(lreturn)
      integer, intent(in) :: stat, check_value, iounit
      integer, optional, intent(inout) :: rc
      integer, optional, intent(in) :: rc_value
      logical :: lreturn

      lreturn = failed(stat, check_value, rc, rc_value)
      if(lreturn) close(iounit)

   end function fail_integer_close

   function fail_logical_close(stat, check_value, iounit, rc, rc_value) result(lreturn)
      integer, intent(in) :: stat, iounit
      logical, intent(in) :: check_value
      integer, optional, intent(inout) :: rc
      integer, optional, intent(in) :: rc_value
      logical :: lreturn

      lreturn = failed(stat, check_value, rc, rc_value)
      if(lreturn) close(iounit)

   end function fail_logical_close

end program overwrite_config
