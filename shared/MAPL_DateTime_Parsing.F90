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
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_DateTime_Parsing
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use, intrinsic :: iso_fortran_env, only: R64 => real64

   implicit none

! PUBLIC =======================================================================

   public :: date_fields
   public :: time_fields
   public :: datetime_duration
   public :: convert_to_ISO8601DateTime
   public :: is_digit
   public :: is_digit_string
   public :: is_positive_digit
   public :: MAX_CHARACTER_LENGTH
   public :: is_time_unit
   public :: get_time_unit
   public :: UNSET_FIELD

   public :: YEAR_TIME_UNIT, MONTH_TIME_UNIT, DAY_TIME_UNIT
   public :: HOUR_TIME_UNIT, MINUTE_TIME_UNIT, SECOND_TIME_UNIT
   public :: TIME_UNIT, NUM_TIME_UNITS, UNKNOWN_TIME_UNIT 

! Comment out the following line for testing.
!   private

   interface operator(.multipleof.)
      module procedure :: multipleof
   end interface

   interface operator(.in.)
      module procedure :: is_in_closed_interval
   end interface

   interface operator(.between.)
      module procedure :: is_in_open_interval
   end interface

   interface operator(.isvalidindexof.)
      module procedure :: valid_index
   end interface

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

!   type :: datetime_fields
!       integer  :: yy   =   0
!       integer  :: mm   =   0
!       integer  :: dd   =   0
!       integer  :: h    =   0
!       integer  :: m    =   0
!       integer  :: s    =   0
!       real     :: sr8  = 0.0
!   contains
!      procedure, public, pass(this) :: as_array => datetime_fields_as_array
!   end type datetime_fields
!
!   interface datetime_fields
!       module procedure :: construct_datetime_fields
!       module procedure :: construct_datetime_fields_array
!   end interface datetime_fields

   ! DATETIME_DURATION: Derived type for communicating datetime durations internally

   type :: datetime_duration
      integer :: year, month, day, hour, minute, second
      real(kind=R64) :: hour_real, minute_real, second_real
   contains
      procedure, pass(this) :: year_is_set
      procedure, pass(this) :: month_is_set
      procedure, pass(this) :: day_is_set
      procedure, pass(this) :: hour_is_set
      procedure, pass(this) :: minute_is_set
      procedure, pass(this) :: second_is_set
      procedure, pass(this) :: hour_is_real
      procedure, pass(this) :: minute_is_real
      procedure, pass(this) :: second_is_real
      procedure, pass(this) :: set_year => set_year_datetime_duration
      procedure, pass(this) :: set_month => set_month_datetime_duration
      procedure, pass(this) :: set_day => set_day_datetime_duration
      procedure, pass(this) :: set_hour => set_hour_datetime_duration
      procedure, pass(this) :: set_minute => set_minute_datetime_duration
      procedure, pass(this) :: set_second => set_second_datetime_duration
      procedure, pass(this) :: set_hour_real => set_hour_real_datetime_duration
      procedure, pass(this) :: set_minute_real => set_minute_real_datetime_duration
      procedure, pass(this) :: set_second_real => set_second_real_datetime_duration
      procedure, pass(this) :: set_real_value_datetime_duration
      procedure, pass(this) :: set_integer_value_datetime_duration
      generic ::  set_value => set_integer_value_datetime_duration, set_real_value_datetime_duration
   end type datetime_duration

   interface datetime_duration
      module procedure :: construct_datetime_duration
   end interface datetime_duration

   interface unset
      module procedure :: unset_integer
      module procedure :: unset_real
   end interface unset

   interface set_field_value
      module procedure :: set_field_value_integer
      module procedure :: set_field_value_real
   end interface set_field_value

   interface is_set
      module procedure :: is_set_integer
      module procedure :: is_set_real
   end interface is_set
   ! END DATETIME_DURATION


   ! TIME_UNIT: enumerators for standard handling of time units (strings, etc)

   enum, bind(c)
      enumerator :: TIME_UNIT = 0
      enumerator :: YEAR_TIME_UNIT = 1
      enumerator :: MONTH_TIME_UNIT = 2
      enumerator :: DAY_TIME_UNIT = 3
      enumerator :: HOUR_TIME_UNIT = 4
      enumerator :: MINUTE_TIME_UNIT = 5
      enumerator :: SECOND_TIME_UNIT = 6
      enumerator :: LAST_TIME_UNIT = SECOND_TIME_UNIT
      enumerator :: UNKNOWN_TIME_UNIT = -1
   end enum

   integer(kind(TIME_UNIT)), parameter :: NUM_TIME_UNITS = LAST_TIME_UNIT

   integer, parameter :: MAX_CHARACTER_LENGTH = 64
   character(len=MAX_CHARACTER_LENGTH), target :: time_units(NUM_TIME_UNITS) = &
      [character(len=MAX_CHARACTER_LENGTH) :: "year", "month", "day", "hour", "minute", "second"]
   ! END TIME_UNIT

   ! UNSET FIELD
   integer, parameter :: UNSET_FIELD = 0

   ! Error handling
   integer, parameter :: INVALID = -1

   ! parameters for processing date, time, and datetime strings
   character(len=10), parameter :: DIGIT_CHARACTERS = '0123456789'

   integer, parameter :: Z = 0


contains

! NUMBER HANDLING PROCEDURES

   ! Return true if factor divides dividend evenly, false otherwise
   pure logical function multipleof(dividend, factor)
      integer, intent(in) :: dividend
      integer, intent(in) :: factor
      ! mod returns the remainder of dividend/factor,
      ! and if it is 0, factor divides dividend evenly
      if(factor /= 0) then ! To avoid divide by 0
          multipleof = mod(dividend, factor) == 0
      else
          multipleof = .FALSE.
      endif

   end function multipleof

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

   ! Check is s is a digit-only string
   logical function is_digit_string(s)
      character(len=*), intent(in) :: s

      is_digit_string = (len_trim(s) > 0) .and. &
         (verify(s(:len_trim(s)), DIGIT_CHARACTERS) == 0)

   end function is_digit_string

   ! Check if c is a positive digit character
   elemental pure logical function is_positive_digit(c)
      character, intent(in) :: c
      is_positive_digit = is_digit(c) .and. (c /= '0')
   end function is_positive_digit

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
   pure function undelimit(string, delimiter) result(undelimited)
      character(len=*), intent(in) :: string
      character, intent(in) :: delimiter
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
      character(len=len_trim(string)) :: ljustified
      character :: ch
      integer :: trimmed_len
      integer :: i, pos

      ljustified = adjustl(string)
      trimmed_len = len_trim(ljustified)
      undelimited = ''
      pos = 0

      do i= 1, trimmed_len
         ch = ljustified(i:i)
         if(is_digit(ch)) then
            pos = pos + 1
            undelimited(pos:pos) = ch
         end if
      end do

   end function undelimit_all

! END LOW-LEVEL STRING PROCESSING PROCEDURES


! LOW-LEVEL DATE & TIME PROCESSING PROCEDURES

   ! Return true if y is a leap year, false otherwise
   pure logical function is_leap_year(y)
      integer, intent(in) :: y
      ! Leap years are years divisible by 400 or (years divisible by 4 and not divisible by 100)
      is_leap_year = (y .multipleof. 400) .or. ((y .multipleof. 4) .and. .not. (y .multipleof. 100))
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

! DATE_FIELDS:

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


! TIME_FIELDS:

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

! DATETIME_FIELDS:

!   pure function construct_datetime_fields(yy, mm, dd, h, m, s, s8) result(fields)
!      integer, optional, intent(in) :: yy, mm, dd, h, m, s
!      real(kind=R64), optional, intent(in) :: s8
!      type(datetime_fields) :: fields
!
!      if(present(yy)) fields % yy = yy
!      if(present(mm)) fields % mm = mm
!      if(present(dd)) fields % dd = dd
!      if(present(h)) fields % h = h
!      if(present(m)) fields % m = m
!
!      if(present(s8)) then
!         fields % s8 = s8
!         fields % s = int(s8)
!      else if(present(s)) then
!         fields % s = s
!         fields % s8 = real(s, R64)
!      end if
!
!   end function construct_datetime_fields
!
!   pure function construct_datetime_fields_array(dur, s8) result(fields)
!      integer, intent(in) :: dur
!      real(R64), optional, intent(in) :: s8
!      type(datetime_fields) :: fields
!      integer :: yy, mm, dd, h, m, s
!
!      yy = dur(1)
!      mm = dur(2)
!      dd = dur(3)
!      h =  dur(4)
!      m =  dur(5)
!
!      if(present(s8)) then
!         fields = datetime_fields(yy = yy, mm = mm, dd = dd, h = h, m = m, s8 = s8)
!         return
!      end if
!
!      fields = datetime_fields(yy = yy, mm = mm, dd = dd, h = h, m = m, s = s)
!
!   end function construct_datetime_fields_array
!
!   pure function datetime_fields_as_array(this) result(array)
!      class(datetime_fields), intent(in) :: this
!      integer :: array(6)
!
!      array = [this % yy, this % mm, this % dd, this % h, this % m, this % s]
!
!   end function datetime_fields_as_array

! DATETIME_DURATION:

   function construct_datetime_duration() result(that)
      type(datetime_duration) :: that

      call unset(that % year)
      call unset(that % month)
      call unset(that % day)
      call unset(that % hour)
      call unset(that % minute)
      call unset(that % second)

      call unset(that % hour_real)
      call unset(that % minute_real)
      call unset(that % second_real)

   end function construct_datetime_duration

! END CONSTRUCTORS


! TYPE-BOUND METHODS

! DATE_FIELDS:

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


! TIME_FIELDS:

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


   ! DATETIME_DURATION:

   logical function year_is_set(this)
      class(datetime_duration), intent(in) :: this
      year_is_set = is_set(this % year)
   end function year_is_set

   logical function month_is_set(this)
      class(datetime_duration), intent(in) :: this
      month_is_set = is_set(this % month)
   end function month_is_set

   logical function day_is_set(this)
      class(datetime_duration), intent(in) :: this
      day_is_set = is_set(this % day)
   end function day_is_set

   logical function hour_is_set(this)
      class(datetime_duration), intent(in) :: this
      hour_is_set = is_set(this % hour) .or. is_set(this % hour_real)
   end function hour_is_set

   logical function minute_is_set(this)
      class(datetime_duration), intent(in) :: this
      minute_is_set = is_set(this % minute) .or. is_set(this % minute_real)
   end function minute_is_set

   logical function second_is_set(this)
      class(datetime_duration), intent(in) :: this
      second_is_set = is_set(this % second) .or. is_set(this % second_real)
   end function second_is_set

   logical function hour_is_real(this)
      class(datetime_duration), intent(in) :: this
      hour_is_real = this % hour_is_set() .and. is_set(this % hour_real) 
   end function hour_is_real

   logical function minute_is_real(this)
      class(datetime_duration), intent(in) :: this
      minute_is_real = this % minute_is_set() .and. is_set(this % minute_real) 
   end function minute_is_real

   logical function second_is_real(this)
      class(datetime_duration), intent(in) :: this
      second_is_real = this % second_is_set() .and. is_set(this % second_real) 
   end function second_is_real

   subroutine set_year_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      integer, intent(in) :: val
      integer, optional, intent(out) :: rc

      this % year = val

      __RETURN(__SUCCESS)

   end subroutine set_year_datetime_duration

   subroutine set_month_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      integer, intent(in) :: val
      integer, optional, intent(out) :: rc

      this % month = val

      __RETURN(__SUCCESS)

   end subroutine set_month_datetime_duration

   subroutine set_day_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      integer, intent(in) :: val
      integer, optional, intent(out) :: rc

      this % day = val

      __RETURN(__SUCCESS)

   end subroutine set_day_datetime_duration

   subroutine set_hour_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      integer, intent(in) :: val
      integer, optional, intent(out) :: rc

      call set_field_value(val, this % hour, this % hour_real)

      __RETURN(__SUCCESS)

   end subroutine set_hour_datetime_duration

   subroutine set_hour_real_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      real(kind=R64), intent(in) :: val
      integer, optional, intent(out) :: rc

      call set_field_value(val, this % hour_real, this % hour)

      __RETURN(__SUCCESS)

   end subroutine set_hour_real_datetime_duration

   subroutine set_minute_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      integer, intent(in) :: val
      integer, optional, intent(out) :: rc

      call set_field_value(val, this % minute, this % minute_real)

      __RETURN(__SUCCESS)

   end subroutine set_minute_datetime_duration

   subroutine set_minute_real_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      real(kind=R64), intent(in) :: val
      integer, optional, intent(out) :: rc

      call set_field_value(val, this % minute_real, this % minute)

      __RETURN(__SUCCESS)

   end subroutine set_minute_real_datetime_duration

   subroutine set_second_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      integer, intent(in) :: val
      integer, optional, intent(out) :: rc

      call set_field_value(val, this % second, this % second_real)

      __RETURN(__SUCCESS)

   end subroutine set_second_datetime_duration

   subroutine set_second_real_datetime_duration(this, val, rc)
      class(datetime_duration), intent(inout) :: this
      real(kind=R64), intent(in) :: val
      integer, optional, intent(out) :: rc

      call set_field_value(val, this % second_real, this % second)

      __RETURN(__SUCCESS)

   end subroutine set_second_real_datetime_duration

   subroutine set_integer_value_datetime_duration(this, tunit, val, rc)
      class(datetime_duration), intent(inout) :: this
      integer(kind(TIME_UNIT)), intent(in) :: tunit
      integer, intent(in) :: val
      integer, optional, intent(out) :: rc

      select case(tunit)
      case (YEAR_TIME_UNIT)
         call this % set_year(val)
      case (MONTH_TIME_UNIT)
         call this % set_month(val)
      case (DAY_TIME_UNIT)
         call this % set_day(val)
      case (HOUR_TIME_UNIT)
         call this % set_hour(val)
      case (MINUTE_TIME_UNIT)
         call this % set_minute(val)
      case (SECOND_TIME_UNIT)
         call this % set_second(val)
      case default
         __FAIL('Invalid Time Unit')
      end select

      __RETURN(__SUCCESS)

   end subroutine set_integer_value_datetime_duration

   subroutine set_real_value_datetime_duration(this, tunit, val, rc)
      class(datetime_duration), intent(inout) :: this
      integer(kind(TIME_UNIT)), intent(in) :: tunit
      real(kind=R64), intent(in) :: val
      integer, optional, intent(out) :: rc

      select case(tunit)
      case (HOUR_TIME_UNIT)
         call this % set_hour_real(val)
      case (MINUTE_TIME_UNIT)
         call this % set_minute_real(val)
      case (SECOND_TIME_UNIT)
         call this % set_second_real(val)
      case default
         __FAIL('Invalid Time Unit')
      end select

      __RETURN(__SUCCESS)

   end subroutine set_real_value_datetime_duration

   ! END CF Time: Type-bound procedues


! END TYPE-BOUND METHODS

   !wdb deleteme Not testing. May not be necessary.
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
      integer, parameter :: N(2, S) = reshape([1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [2, S])
      integer, parameter :: MIN_LEN = 14
      character(len=*), parameter :: ISO_DD = '-'
      character(len=*), parameter :: ISO_TD = ':'
      character(len=*), parameter :: ISO_DTD = 'T'
      character(len=*), parameter :: ISO_POINT = '.'
      character(len=len(datetime_string)) :: undelimited
      character(len=:), allocatable :: intermediate
      integer :: undelimited_length

      iso_string = datetime_string
      undelimited = adjustl(undelimit_all(datetime_string))
      undelimited_length=len_trim(undelimited)
      if(undelimited_length < MIN_LEN) then
         __RETURN(__FAILURE)
      end if

      intermediate = undelimited(N(1,YY):N(2,YY)) // ISO_DD // &
                     undelimited(N(1,MM):N(2,MM)) // ISO_DD // &
                     undelimited(N(1,DD):N(2,DD)) // ISO_DTD // &
                     undelimited(N(1,HH):N(2,HH)) // ISO_TD // &
                     undelimited(N(1,M):N(2,M)) // ISO_TD // &
                     undelimited(N(1,S):N(2,S))
      if(undelimited_length > MIN_LEN) intermediate = &
          intermediate // ISO_POINT // undelimited(MIN_LEN+1:undelimited_length)

      iso_string = intermediate

      __RETURN(__SUCCESS)

   end subroutine convert_to_ISO8601DateTime


   ! UTILITY PROCEDURES

   function is_valid_datestring(datestring, string_format) result(tval)
      character(len=*), intent(in) :: datestring
      character(len=*), intent(in) :: string_format
      logical :: tval
      integer :: i

      tval = .false.

      if(len(datestring) /= len(string_format)) return

      do i = 1, len_trim(string_format)
         if(is_digit(string_format(i:i))) then
            if(.not. is_digit(datestring(i:i))) return
         else
            if(datestring(i:i) /= string_format(i:i)) return
         end if
      end do

      tval = .true.

   end function is_valid_datestring

   logical function is_in_char_set(element, char_set)
      character, intent(in) :: element
      character(len=*), intent(in) :: char_set
      is_in_char_set = (verify(element, char_set) == 0)
   end function is_in_char_set

   function find_delta(string, chars, istart, istop_in) result(next)
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: chars
      integer, intent(in) :: istart
      integer, optional, intent(in) :: istop_in
      integer :: next
      logical :: in_set
      integer :: istop

      if(len(chars) == 0) return
      if(istart < 1) return

      istop = len(string)
      if(istop == 0) return

      if(present(istop_in)) then
         if(istop_in > istop) return
         istop = istop_in
      end if

      if(istop < istart) return

      next = istart
      in_set = is_in_char_set(string(next:next), chars)

      do
         next = next + 1
         if(next > len(string)) exit
         if(in_set .neqv. is_in_char_set(string(next:next), chars)) exit
      end do

   end function find_delta

   function find_delta_datestring(string, istart, istop) result(next)
      character(len=*), intent(in) :: string
      integer, intent(in) :: istart
      integer, optional, intent(in) :: istop
      integer :: istop_
      integer :: next

      istop_ = len(string)
      if(present(istop)) istop_ = istop
      next = find_delta(string, DIGIT_CHARACTERS, istart, istop)

   end function find_delta_datestring

   subroutine split_digit_string_delimited(string, parts, rc)
      character(len=*), intent(in) :: string
      class(StringVector), intent(inout) :: parts
      integer, optional, intent(out) :: rc
      integer :: next, start, strlen, last

      strlen = len(string)
      if(strlen == 0) then
         __FAIL('Empty string')
      end if

      start = 1
      do
         next = find_delta_datestring(string, start)
         if(.not. (next > start)) exit
         last = next - 1
         if(last <= strlen) then
            __FAIL('Exceeded string length')
         end if
         call parts % push_back(string(start:(next-1)))
         start = next
         if(start > len(string)) exit
      end do

      __RETURN(__SUCCESS)

   end subroutine split_digit_string_delimited

   logical function valid_index(n, string)
      integer, intent(in) :: n
      character(len=*), intent(in) :: string

      valid_index = .not. (n < 1 .or. n > len(string))

   end function valid_index

   subroutine split_digit_string_indexed(string, length, parts, rc)
      character(len=*), intent(in) :: string
      integer, intent(in) :: length(:)
      class(StringVector), intent(inout) :: parts
      integer, optional, intent(out) :: rc
      integer, allocatable :: indices(:, :)
      integer :: i
      integer :: n(2)

      indices = convert_lengths_to_indices(length)

      do i = 1, size(indices, 2)
         n = indices(:,i)
         call parts % push_back(string(n(1):n(2)))
      end do

      __RETURN(__SUCCESS)

   end subroutine split_digit_string_indexed

   function convert_lengths_to_indices(length) result(indices)
      integer, intent(in) :: length(:)
      integer :: indices(2, size(length))
      integer :: i

      indices(:, 1) = [1, length(1)]
      do i = 2, size(indices, 2)
         indices(:, i) = indices(:,(i-1)) + [1, length(i)]
      end do

   end function convert_lengths_to_indices

   subroutine unset_integer(n)
      integer, intent(out) :: n
      n = UNSET_FIELD
   end subroutine unset_integer

   subroutine unset_real(t)
      real(kind=R64), intent(out) :: t
      t = real(UNSET_FIELD, kind=R64)
   end subroutine unset_real

   logical function is_set_integer(n) 
      integer, intent(in) :: n
      is_set_integer = (n /= UNSET_FIELD)
   end function is_set_integer

   logical function is_set_real(t)
      real(kind=R64), intent(in) :: t
      is_set_real = (t /= real(UNSET_FIELD, kind=R64))
   end function is_set_real

   subroutine set_field_value_integer(new_value, integer_value, real_value)
      integer, intent(in) :: new_value
      integer, intent(out) :: integer_value
      real(kind=R64), intent(out) :: real_value

      integer_value = new_value
      call unset(real_value)
      
   end subroutine set_field_value_integer

   subroutine set_field_value_real(new_value, real_value, integer_value)
      real(kind=R64), intent(in) :: new_value
      real(kind=R64), intent(out) :: real_value
      integer, intent(out) :: integer_value

      real_value = new_value
      call unset(integer_value)
      
   end subroutine set_field_value_real

! TIME_UNIT ====================================================================

   function get_time_unit(unit_name, check_plural) result(unit_num)
      character(len=*), intent(in) :: unit_name
      logical, optional, intent(in) :: check_plural
      integer(kind(TIME_UNIT)) :: unit_num
      character(len=:), allocatable  :: unit_name_
      logical :: check_plural_ = .TRUE.
      character(len=:), pointer, save :: tunits(:)
      character(len=:), allocatable :: tunit
      character, parameter :: PLURAL = 's'
      integer(kind(TIME_UNIT)) :: i
      character(len=*), parameter :: IFMT = '(A,I1)'
      character(len=*), parameter :: LFMT = '(A,L)'

      check_plural_ = .TRUE.

      if(present(check_plural)) check_plural_ = check_plural

      unit_name_ = trim(unit_name)
      tunits => time_units

      unit_num = UNKNOWN_TIME_UNIT
      do i = 1, NUM_TIME_UNITS
         tunit = trim(tunits(i))
         if((tunit == unit_name_) .or. (check_plural_ .and. ((tunit // PLURAL) == unit_name_))) then
            unit_num = i
            exit
         end if
      end do

   end function get_time_unit

   logical function is_time_unit(string)
      character(len=*), intent(in) :: string
      is_time_unit = (get_time_unit(string) /= UNKNOWN_TIME_UNIT)
   end function is_time_unit

end module MAPL_DateTime_Parsing
