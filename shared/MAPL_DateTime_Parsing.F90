! MAPL Date/Time Parsing
!
! This module implements general parsing of strings representing dates and times
! Zero-padded strings must be zero padded to their full width, unless
! otherwise specified.
! In the following:
! ? is either a zero-width character or a delimiter like '-' or ':'
!
! Date
!      YYYY
! Years are represented by 4 numerical characters (Y).
! The range is: '0000' to '9999' representing the years 1 BCE ('0000') to
! 9999 CE ('9999').
!
! These date formats are supported.
!      YYYY?MM?DD or YYYYMMDD
!      YYYY?MM (but not YYYYMM)
! YYYY is the year as specified above.
! MM is the zero-padded month from '01' (January) to '12' (December).
! DD is the zero-padded day of the month from '01' (1) to '31' (31).
! The range of allowed DD strings varies according to the actual
! calendar, though the first day is always '01'.
!
! Time
! This module supports fractional seconds. (currently only milliseconds)
!      hh?mm?ss.sss	or	Thhmmss.sss
!      hh?mm?ss	or	Thhmmss
!      hh?mm	or	Thhmm
!      hh
! hh is the zero-padded hour (24 hour system).
! mm is the zero-padded minute. 
! ss is the zero-padded second.
! sss is the fractional second. It represents an arbitrary number of digits (currrently limited to 3).
!
! Fully-formed time with time zone. Local time not-supported
!      <time>Z

!wdb fixme need to enforce private (see commented out private statements and add to other type/variables
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_DateTime_Parsing
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none

!   private 
   public :: date_fields
   public :: time_fields

   interface operator(.divides.)
      module procedure :: divides
   end interface

   interface operator(.in.)
      module procedure :: is_in_closed_interval
   end interface

   interface operator(.between.)
      module procedure :: is_in_open_interval
   end interface 

   ! Error handling
   integer, parameter :: INVALID = -1

   ! parameters for processing date, time, and datetime strings
   character(len=10), parameter :: DIGIT_CHARACTERS = '0123456789'

   ! Timezone offset for Timezone Z !wdb keep for now
   integer, parameter :: Z = 0

   ! Derived type for communicating date fields internally
   type :: date_fields
      integer :: year_
      integer :: month_
      integer :: day_
      logical :: is_valid_ = .FALSE.
   contains
      procedure, public, pass(this) :: year => get_year_field
      procedure, public, pass(this) :: month => get_month_field
      procedure, public, pass(this) :: day => get_day_field
      procedure, public, pass(this) :: is_valid => are_valid_date_fields
   end type date_fields

   interface date_fields
      module procedure :: construct_date_fields_default
      module procedure :: construct_date_fields_string
      module procedure :: construct_date_fields_null
   end interface date_fields

   ! Derived type for communicating time fields internally
   type :: time_fields
      integer :: hour_
      integer :: minute_
      integer :: second_
      integer :: millisecond_
      integer :: timezone_offset_
      logical :: is_valid_ = .FALSE.
   contains
      procedure, public, pass(this) :: hour => get_hour_field
      procedure, public, pass(this) :: minute => get_minute_field
      procedure, public, pass(this) :: second => get_second_field
      procedure, public, pass(this) :: millisecond => get_millisecond_field
      procedure, public, pass(this) :: timezone_offset => get_timezone_offset_field
      procedure, public, pass(this) :: is_valid => are_valid_time_fields
   end type time_fields

   interface time_fields
      module procedure :: construct_time_fields_default
      module procedure :: construct_time_fields_string
      module procedure :: construct_time_fields_null
   end interface time_fields

contains

! NUMBER HANDLING PROCEDURES

   ! Return true if factor divides dividend evenly, false otherwise
   pure logical function divides(factor, dividend)
      integer, intent(in) :: factor
      integer, intent(in) :: dividend
      ! mod returns the remainder of dividend/factor, and if it is 0, factor divides dividend evenly
      if(factor /= 0) then ! To avoid divide by 0
          divides = mod(dividend, factor)==0
      else
          divides = .FALSE.
      endif
   end function divides

   pure logical function is_in_closed_interval(n, clint)
      integer, intent(in) :: n
      integer, intent(in) :: clint(2)
      is_in_closed_interval = .not. ((n < clint(1)) .or. (n > clint(2)))
   end function is_in_closed_interval

   pure logical function is_in_open_interval(n, opint)
      integer, intent(in) :: n
      integer, intent(in) :: opint(2)
      is_in_open_interval = (n > opint(1)) .and. (n < opint(2))
   end function is_in_open_interval

   ! Check if c is a digit character
   elemental pure logical function is_digit(c)
      character, intent(in) :: c
      is_digit = scan(c, DIGIT_CHARACTERS) > 0
   end function is_digit

   ! Check if n is an integer >= 0
   pure logical function is_whole_number(n)
      integer, intent(in) :: n
      is_whole_number = .not. (n < 0)
   end function is_whole_number

   ! Convert c from digit character to integer
   pure integer function get_integer_digit(c)
      character, intent(in) :: c
      get_integer_digit = index(DIGIT_CHARACTERS, c) - 1
   end function get_integer_digit

   ! Convert index i character from s to integer
   pure integer function get_integer_digit_from_string(s, i)
      character(len=*), intent(in) :: s
      integer, intent(in) :: i

      if((i>0) .and. (i<len(s)+1)) then
         get_integer_digit_from_string = get_integer_digit(s(i:i))
      else
         get_integer_digit_from_string = INVALID
      end if
   end function get_integer_digit_from_string

   ! Convert string to whole number
   pure integer function read_whole_number(string)
      character(len=*), intent(in) :: string
      read_whole_number = read_whole_number_indexed(string, 1, len(string))
   end function read_whole_number

   ! Convert string(istart: istop) to whole number
   pure integer function read_whole_number_indexed(string, istart, istop)
      character(len=*), intent(in) :: string
      integer, intent(in) :: istart
      integer, intent(in) :: istop
      integer, parameter :: BASE=10
      integer :: n
      integer :: i
      integer :: place_value
      integer :: digit_value

      read_whole_number_indexed = INVALID

      ! Check indices
      if((istart < 1) .or. (istart > istop) .or. (istop > len(string))) return

      ! Convert characters from string, last to first, to integers,
      ! multiplies by place value, and adds
      place_value = 1
      n = 0
      do i= istop, istart, -1
         digit_value = get_integer_digit_from_string(string, i)
         if(is_whole_number(digit_value)) then
            n = n + digit_value * place_value
            place_value = place_value * BASE
         else
            n = INVALID
            exit
         end if
      end do
      read_whole_number_indexed = n
   end function read_whole_number_indexed

! END NUMBER HANDLING PROCEDURES

! LOW-LEVEL STRING PROCESSING PROCEDURES

   ! Strip delimiter from string
   !wdb todo should this be more than 1 character
   pure function undelimit(string, delimiter) result(undelimited)
      character(len=*), intent(in) :: string
      character,  intent(in) :: delimiter
      character(len=len(string)) :: undelimited
      integer :: i
      integer :: j
      
      undelimited = string
      if(len_trim(delimiter) <= 0) return

      undelimited = ''
      j = 0
      do i=1,len(string)
        if(string(i:i) == delimiter) cycle
        j = j + 1
        undelimited(j:j) = string(i:i)
      end do
   end function undelimit

   pure function undelimit_all(string) result(undelimited)
      character(len=*), intent(in) :: string
      character(len=len_trim(string)) :: undelimited
      character(len=len_trim(string)) :: trimmed
      character :: ch
      integer :: i

      trimmed = trim(adjustl(string))
      undelimited = ''

      do i= 1, len(trimmed)
         ch = trimmed(i:i)
         if(is_digit(ch)) undelimited = undelimited // ch
      end do

   end function undelimit_all

! END LOW-LEVEL STRING PROCESSING PROCEDURES


! LOW-LEVEL DATE & TIME PROCESSING PROCEDURES

   ! Return true if y is a leap year, false otherwise
   pure logical function is_leap_year(y)
      integer, intent(in) :: y
      ! Leap years are years divisible by 400 or (years divisible by 4 and not divisible by 100)
      is_leap_year = (400 .divides. y) .or. ((4 .divides. y) .and. .not. (100 .divides. y))
   end function is_leap_year

   ! Return the last day numbers of each month based on the year
   pure function get_month_ends(y) result(month_ends)
      integer, intent(in) :: y
      integer, parameter :: NUM_MONTHS = 12
      integer, dimension(NUM_MONTHS) :: month_ends
      ! last day numbers of months for leap years
      integer, dimension(NUM_MONTHS) :: MONTH_END_LEAP
      ! last day numbers of months for regular years
      integer, dimension(NUM_MONTHS) :: MONTH_END
      MONTH_END = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      MONTH_END_LEAP = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      if(is_leap_year(y)) then
         month_ends = MONTH_END_LEAP
      else
         month_ends = MONTH_END
      endif
   end function get_month_ends

   ! Return the last day number of month m in year y
   pure integer function get_month_end(y, m)
      integer, intent(in) :: y
      integer, intent(in) :: m
      integer, dimension(:), allocatable :: month_ends

      month_ends = get_month_ends(y)
      get_month_end = month_ends(m)

   end function get_month_end

   ! Verify that y is a valid year
   pure logical function is_valid_year(y)
      integer, intent(in) :: y

      is_valid_year = y .in. [0, 9999]
   end function is_valid_year

   ! Verify that m is a valid month number
   pure logical function is_valid_month(m)
      integer, intent(in) :: m
      is_valid_month = m .in. [1, 12]
   end function is_valid_month

   ! Verify that day d in in month m of year y
   pure logical function is_valid_day(y, m, d)
      integer, intent(in) :: y
      integer, intent(in) :: m
      integer, intent(in) :: d
      is_valid_day = d .in.  [1, get_month_end(y, m)]
   end function is_valid_day

   ! Verify that hour is a valid hour
   pure logical function is_valid_hour(hour)
      integer, intent(in) :: hour
      is_valid_hour = hour .in. [0, 23]
   end function is_valid_hour

   ! Verify that minute is a valid minute
   pure logical function is_valid_minute(minute)
      integer, intent(in) :: minute
      is_valid_minute = minute .in. [0, 59]
   end function is_valid_minute

   ! Verify that second is a valid second
   pure logical function is_valid_second(second)
      integer, intent(in) :: second
      is_valid_second = second .in. [0, 60]
   end function is_valid_second

   ! Verify that millisecond is a valid millisecond values
   pure logical function is_valid_millisecond(millisecond)
      integer, intent(in) :: millisecond
      is_valid_millisecond = millisecond .in. [0, 999]
   end function is_valid_millisecond

   ! Verify that the timezone offset is valid
   ! Not strict. Just looks for a 'reasonable' offset (+/- 1440 minutes = 24 * 60 minutes)
   pure logical function is_valid_timezone_offset(timezone_offset)
      integer, parameter :: MAX_OFFSET = 1440
      integer, intent(in) :: timezone_offset
      is_valid_timezone_offset = timezone_offset .in. [-MAX_OFFSET, MAX_OFFSET]
   end function is_valid_timezone_offset

! END LOW-LEVEL DATE & TIME PROCESSING PROCEDURES


! DATE & TIME VERIFICATION

   ! Verify all the date fields are valid
   pure logical function is_valid_date(date)
      type(date_fields), intent(in) :: date

      is_valid_date = is_valid_year(date%year()) .and. &
         is_valid_month(date%month()) .and. &
         is_valid_day(date%year(), date%month(), date%day())

   end function is_valid_date

   ! Verify all the time fields are valid
   pure logical function is_valid_time(time)
      type(time_fields), intent(in) :: time

      is_valid_time = is_valid_hour(time%hour()) .and. &
         is_valid_minute(time%minute()) .and. &
         is_valid_second(time%second()) .and. &
         is_valid_millisecond(time%millisecond()) .and. &
         is_valid_timezone_offset(time%timezone_offset())

   end function is_valid_time

! END DATE & TIME VERIFICATION


! STRING PARSERS

   ! parse string representing a timezone offset (absolute value)
   ! return integer offset in minutes, or on error return negative number
   pure function parse_timezone_offset(offset, field_width) result(tzo)
      character(len=*), intent(in) :: offset
      integer, intent(in) :: field_width
      integer :: tzo
      integer, parameter :: MINUTES_PER_HOUR = 60
      integer :: offset_length
      integer :: minutes
      integer :: hours

      offset_length = len_trim(offset)

      if(offset_length == field_width) then
         ! Offset with hours only
         hours = read_whole_number(offset)
         tzo = hours*MINUTES_PER_HOUR
      elseif (offset_length == 2*field_width) then
         ! Offset with hours and minutes
         hours = read_whole_number(offset(1:field_width))
         minutes = read_whole_number(offset(field_width+1:len(offset)))
         if(is_whole_number(hours) .and. is_whole_number(minutes)) then
            tzo = hours*MINUTES_PER_HOUR + minutes
         else
            tzo = INVALID
         end if
      else
         tzo = INVALID
      end if

   end function parse_timezone_offset

   ! Parse ISO 8601 Date string into date fields, check if valid date
   ! and set is_valid_ flag
   pure function parse_date(datestring, delimiter) result(fields)
      character(len=*), intent(in) :: datestring
      character, intent(in) :: delimiter
      type(date_fields) :: fields
      integer, parameter :: LENGTH = 8
      integer, parameter :: YEAR_POSITION = 1
      integer, parameter :: MONTH_POSITION = 5
      integer, parameter :: DAY_POSITION = 7
      integer :: year, month, day
      character(len=LENGTH) :: undelimited

      ! initialize fields to empty (is_valid_ .FALSE.)
      fields = date_fields()

      ! Eliminate delimiters so that there is one parsing block
      undelimited=undelimit(datestring, DELIMITER)

      if(len(undelimited) /= LENGTH) return

      year = read_whole_number(undelimited(YEAR_POSITION:MONTH_POSITION-1))
      month = read_whole_number(undelimited(MONTH_POSITION:DAY_POSITION-1))
      day = read_whole_number(undelimited(DAY_POSITION:LENGTH))
      fields = date_fields(year, month, day)

   end function parse_date

   ! Parse ISO 8601 Time string into time fields, check if valid time
   ! and set is_valid_ flag
   pure function parse_time(timestring, delimiter) result(fields)
      character(len=*), intent(in) :: timestring
      character, optional, intent(in) :: delimiter
      type(time_fields) :: fields

      character(len=:), allocatable :: timestring_
      integer, parameter :: LENGTH = 6
      character, parameter :: DECIMAL_POINT = '.'
      integer, parameter :: FIELDWIDTH = 2
      integer, parameter :: MS_WIDTH = 3
      integer :: pos
      character(len=:), allocatable :: undelimited
      character :: c
      character(len=LENGTH) :: offset
      integer :: offset_minutes
      integer :: undelimited_length
      integer :: signum
      integer :: hour
      integer :: minute
      integer :: second
      integer :: millisecond
      integer :: timezone_offset

      fields = time_fields()
      
      timestring_ = trim(timestring)

      ! Get timezone
      ! Find timezone portion
      pos = scan(timestring_, '-Z+')

      if(.not. pos > 0) return

      c = timestring_(pos:pos)

      ! Check first character of timezone portion
      select case(c)
         case('Z')
            signum = 0
         case('-')
            signum = -1
         case('+')
            signum = +1
         case default
            return
      end select

      ! Set timezone offset
      if(signum == 0) then
         if(pos /= len(timestring_)) return
         timezone_offset = Z
      else
         offset = timestring_(pos+1:len(timestring_))
         offset = undelimit(offset, delimiter)
         offset_minutes = parse_timezone_offset(offset, FIELDWIDTH)
         if(.not. is_whole_number(offset_minutes)) return
         timezone_offset = signum * offset_minutes
      end if

      ! Select portion starting at fields%hour and ending before timezone
      undelimited = adjustl(timestring_(1:pos-1))

      ! Remove delimiter and decimal point
      undelimited = undelimit(undelimited, delimiter)
      undelimited=trim(undelimit(undelimited, DECIMAL_POINT))
      undelimited_length = len(undelimited)

      ! Check length of undelimited string with or without milliseconds
      select case(undelimited_length)
         case(LENGTH)
            millisecond = 0
         case(LENGTH+MS_WIDTH)
            millisecond = read_whole_number(undelimited(7:9))
         case default
            return
      end select

      ! Read time fields      
      hour = read_whole_number(undelimited(1:2))
      minute = read_whole_number(undelimited(3:4))
      second = read_whole_number(undelimited(5:6))

      fields = time_fields(hour, minute, second, millisecond, timezone_offset)

   end function parse_time
! END STRING PARSERS


! CONSTRUCTORS

   pure function construct_date_fields_default(year, month, day) result(fields)
      integer, intent(in) :: year
      integer, intent(in) :: month
      integer, intent(in) :: day
      type(date_fields) :: fields
      fields%year_ = year
      fields%month_ = month
      fields%day_ = day
      fields%is_valid_ = is_valid_date(fields)
   end function construct_date_fields_default

   pure function construct_date_fields_string(date_string, delimiter) result(fields)
      character(len=*), intent(in) :: date_string
      character(len=*), intent(in) :: delimiter
      type(date_fields) :: fields
      fields = parse_date(date_string, delimiter)
   end function construct_date_fields_string

   pure function construct_date_fields_null() result(fields)
      type(date_fields) :: fields
      fields%is_valid_ = .FALSE.
   end function construct_date_fields_null

   pure function construct_time_fields_default(hour, minute, second, millisecond, &
      timezone_offset) result(fields)
      integer, intent(in) :: hour
      integer, intent(in) :: minute
      integer, intent(in) :: second
      integer, intent(in) :: millisecond
      integer, intent(in) :: timezone_offset
      type(time_fields) :: fields
      fields%hour_ = hour
      fields%minute_ = minute
      fields%second_ = second
      fields%millisecond_ = millisecond
      fields%timezone_offset_ = timezone_offset
      fields%is_valid_ = is_valid_time(fields)
   end function construct_time_fields_default

   pure function construct_time_fields_string(time_string, delimiter) result(fields)
      character(len=*), intent(in) :: time_string
      character(len=*), intent(in) :: delimiter
      type(time_fields) :: fields
      fields = parse_time(time_string, delimiter)
   end function construct_time_fields_string

   pure function construct_time_fields_null() result(fields)
      type(time_fields) :: fields
      fields%is_valid_ = .FALSE.
   end function construct_time_fields_null

! END CONSTRUCTORS


! TYPE-BOUND METHODS

   pure integer function get_year_field(this)
      class(date_fields), intent(in) :: this
      get_year_field = this%year_
   end function get_year_field

   pure integer function get_month_field(this)
      class(date_fields), intent(in) :: this
      get_month_field = this%month_
   end function get_month_field

   pure integer function get_day_field(this)
      class(date_fields), intent(in) :: this
      get_day_field = this%day_
   end function get_day_field

   pure logical function are_valid_date_fields(this)
      class(date_fields), intent(in) :: this
      are_valid_date_fields = this%is_valid_
   end function are_valid_date_fields

   pure integer function get_hour_field(this)
      class(time_fields), intent(in) :: this
      get_hour_field = this%hour_
   end function get_hour_field

   pure integer function get_minute_field(this)
      class(time_fields), intent(in) :: this
      get_minute_field = this%minute_
   end function get_minute_field

   pure integer function get_second_field(this)
      class(time_fields), intent(in) :: this
      get_second_field = this%second_
   end function get_second_field

   pure integer function get_millisecond_field(this)
      class(time_fields), intent(in) :: this
      get_millisecond_field = this%millisecond_
   end function get_millisecond_field

   pure integer function get_timezone_offset_field(this)
      class(time_fields), intent(in) :: this
      get_timezone_offset_field = this%timezone_offset_
   end function get_timezone_offset_field

   pure logical function are_valid_time_fields(this)
      class(time_fields), intent(in) :: this
      are_valid_time_fields = this%is_valid_
   end function are_valid_time_fields

   subroutine convert_to_ISO8601DateTime(datetime_string, iso_string, rc)
      character(len=*), intent(in) :: datetime_string
      character(len=:), allocatable, intent(out) :: iso_string
      integer, optional, intent(out) :: rc
      integer, parameter :: YY = 1
      integer, parameter :: MM = 2
      integer, parameter :: DD = 3
      integer, parameter :: HH = 4
      integer, parameter :: M = 5
      integer, parameter :: S = 6
      integer, parameter :: N(S, 2) = reshape([1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [S, 2])
      integer, parameter :: MIN_LEN = 14
      character(len=*), parameter :: ISO_DD = '-'
      character(len=*), parameter :: ISO_TD = ':'
      character(len=*), parameter :: ISO_DTD = 'T'
      character(len=*), parameter :: ISO_POINT = '.'
      character(len=:), allocatable :: undelimited
      integer :: undelimited_length
      integer :: status
      
      undelimited = trim(adjustl(undelimit_all(datetime_string)))
      undelimited_length=len(undelimited)
      _ASSERT(undelimited_length >= MIN_LEN, 'datetime_string is too short')
      iso_string =   undelimited(N(YY,1):N(YY,2)) // ISO_DD // &
                     undelimited(N(MM,1):N(MM,2)) // ISO_DD // &
                     undelimited(N(DD,1):N(DD,2)) // ISO_DTD // &
                     undelimited(N(HH,1):N(HH,2)) // ISO_TD // &
                     undelimited(N(M,1):N(M,2)) // ISO_TD // &
                     undelimited(N(S,1):N(S,2))
      if(undelimited_length > MIN_LEN) iso_string = iso_string // ISO_POINT // undelimited(MIN_LEN+1:undelimited_length)

   end subroutine convert_to_ISO8601DateTime 

end module MAPL_DateTime_Parsing
