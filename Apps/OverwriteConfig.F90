program overwrite_config

   use, intrinsic :: iso_fortran_env, only: R64 => real64
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
   integer :: stat

   call main(rc=stat)
   if(stat /= 0) write(*, fmt=FMT_) 'Failure'

contains

   subroutine main(rc)
      integer, optional, intent(out) :: rc
      integer :: stat, iounit
      type(ESMF_Config) :: config
      character(len=MAX_LENGTH) :: filename
      character(len=MAX_LENGTH) :: message
      logical :: test_passed

      filename = 'temp_file'
      call setup(config, filename, iounit, rc = stat)
      if(fail_close(stat, SUCCESS, rc)) then
         write(*, fmt=FMT_) 'Setup failed'
         test_passed = .FALSE.
      end if

      if(test_passed) then
         call test_overwrite(config, LABEL1, VALUE1, LABEL2, test_passed, message, rc=stat)
      end if

      if(fail_close(stat, .not. test_passed, rc)) write(*, fmt=FMT_) trim(message)

      if(test_passed) then
         if (fail_close(stat, SUCCESS, rc)) write(*, fmt=FMT_) 'test_overwrite failed.'
      end if

   end subroutine main

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
      if(fail_close(ios, IOSUCCESS, iounit, rc=stat)) return

      call write_attributes(build_attributes(), iounit, rc=stat)
      if(fail_close(stat, SUCCESS, iounit, rc=stat)) return

      call ESMF_ConfigLoadFile(config, trim(filename),  rc=stat)
      if(fail_close(stat, SUCCESS, iounit, rc=stat)) return

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
      character, parameter :: DELIMITER = ' '
      character(len=MAX_LENGTH) :: line
      logical :: is_open
      integer :: ios, i

      inquire(unit=iounit, opened=is_open, iostat=ios)
      if(failed(ios, IOSUCCESS, rc, FAILURE)) return
      if(failed(ios, .not. is_open, rc, FAILURE)) return

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

      lreturn = failed(stat, stat /= check_value, rc=rc, rc_value=rc_value)

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

   integer function randint(low, high, seed)
      integer, intent(in) :: low, high
      integer, optional, intent(in) :: seed

      real(R64) :: harvest

      call random_number(harvest)

      randint = low + floor((high - low + 1) * harvest)

   end function randint

   subroutine create_restricted_character_set(chars)
      character, intent(inout) :: chars(:)
      integer :: i, j, k, m, sizes
      integer, parameter :: RANGE_SIZE = 2
      integer :: ranges(RANGE_SIZE, 5)

      ranges = reshape([ &
            iachar('0'), iachar('9'), &
            iachar('A'), iachar('Z'), &
            iachar('a'), iachar('z'), &
            iachar('-'), iachar('-'), &
            iachar('_'), iachar('_')  &
         ], shape(ranges))

      sizes = size(chars)
      chars = '-_'
      if(sizes < 3) return

      i = 2

      do k = 1, size(ranges, 2)
         m = min(ranges(2, k), 0)
         do j = min(ranges(1, k), m), m
            i = i + 1
            if(i > sizes) return
            chars(i) = achar(j)
         end do
      end do

   end subroutine create_restricted_character_set

   character function generate_random_character(restricted) result(c)
      logical, optional, intent(in) :: restricted
      character, save :: character_set(64) = EMPTY_STRING
      integer, save :: size_chars
      logical :: restricted_ = .TRUE.

      if(present(restricted)) restricted_ = restricted
      
      if(restricted) then
         if(all(character_set == '')) then
             call create_restricted_character_set(character_set)
             size_chars = size(character_set)
         end if
         c = character_set(randint(1, size(character_set)))
      end if
      
   end function generate_random_character

   function generate_random_characters(length, restricted) result(chars)
      integer, intent(in) :: length
      logical, optional, intent(in) :: restricted
      character(len=length) :: chars
      integer :: i

      chars = EMPTY_STRING

      do i = 1, length
         chars(i:i) = generate_random_character(restricted)
      end do

   end function generate_random_characters

end program overwrite_config
