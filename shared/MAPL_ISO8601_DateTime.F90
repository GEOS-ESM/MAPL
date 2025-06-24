! ISO 8601 Date/Time Handling
! This module implements the ISO 8601 standard for date/time strings.
! Specifically, it implements ISO 8601-1:2019, but not ISO 8601-1:2019-2
! Zero-padded strings must be zero padded to their full width, unless
! otherwise specified.
!
!      YYYY
! Years are represented by 4 numerical characters (Y).
! The range is: '0000' to '9999' representing the years 1 BCE ('0000') to
! 9999 CE ('9999').
! Strict ISO 8601-1:2019 compliance disallows years before 1583 CE.
! Strict ISO 8601-1:2019 compliance can be enabled with 'STRICT_ISO8601'.
!
!
!      ±YYYYY
! The optional ±YYYYY extension is not supported.
!
! These date formats are supported.
!      YYYY-MM-DD or YYYYMMDD
!      YYYY-MM (but not YYYYMM)
! YYYY is the year as specified above.
! MM is the zero-padded month from '01' (January) to '12' (December).
! DD is the zero-padded day of the month from '01' (1) to '31' (31).
! The range of allowed DD strings varies according to the actual
! calendar, though the first day is always '01'.
!
! These formats are not supported (calendar week and ordinal day).
!      YYYY-Www	or	YYYYWww
!      YYYY-Www-D	or	YYYYWwwD
!      YYYY-DDD	or	YYYYDDD
!
! Time
! ISO 8601 allows fractional seconds, but it does not limit the fractional
! part to milliseconds. Below, 'sss' represents an arbitrary number of digits.
!      Thh:mm:ss.sss	or	Thhmmss.sss
!      Thh:mm:ss	or	Thhmmss
!      Thh:mm	or	Thhmm
!      Thh
!
! Fully-formed time with time zone. Local time not-supported
!      <time>Z
! These time zone formats are not implemented.
!      <time>±hh:mm
!      <time>±hhmm
!      <time>±hh
!
! Datetime
!      <date>T<time>
!
! ISO 8601 Durations
!      PnYnMnDTnHnMnS
!
! These ISO 8601 Duration formats are not supported.
!      PnW
!      P<date>T<time>
!
! ISO 8601 Interval
!      <start>/<end>
!      <start>/<duration>
!      <duration>/<end>
!      <duration>
!
! Repeating ISO 8601 Intervals are not supported.
!      Rn/<interval>
!      R/<interval>

!#define STRICT_ISO8601 !Uncomment for strict ISO8601 compliance
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ISO8601_DateTime
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none

! For testing private methods, leave the following line commented out.
!   private

   public :: convert_ISO8601_to_integer_time
   public :: convert_ISO8601_to_integer_date

   interface operator(.divides.)
      module procedure :: divides
   end interface

   ! Error handling
   integer, parameter :: INVALID = -1

   ! parameters for processing date, time, and datetime strings
   integer, parameter :: NUM_DATE_FIELDS = 3
   integer, parameter :: NUM_TIME_FIELDS = 5
   character(len=10), parameter :: DIGIT_CHARACTERS = '0123456789'
   character, parameter :: TIME_PREFIX = 'T'
   integer, parameter :: MINUTES_PER_HOUR = 60

   ! Timezone offset for Timezone Z
   integer, parameter :: Z = 0

   ! Constants for converting ISO 8601 datetime to integer format
   integer, parameter :: ID_WIDTH = 100
   integer, parameter :: ID_DAY = 1
   integer, parameter :: ID_MONTH = ID_WIDTH * ID_DAY
   integer, parameter :: ID_YEAR = ID_WIDTH * ID_MONTH

   integer, parameter :: IT_WIDTH = ID_WIDTH
   integer, parameter :: IT_SECOND = 1
   integer, parameter :: IT_MINUTE = IT_WIDTH * IT_SECOND
   integer, parameter :: IT_HOUR = IT_WIDTH * IT_MINUTE

   ! Derived type used to store fields from ISO 8601 Dates
   type :: ISO8601Date
      private
      integer :: year_
      integer :: month_
      integer :: day_
   contains
      procedure, public :: get_year
      procedure, public :: get_month
      procedure, public :: get_day
   end type ISO8601Date

   interface ISO8601Date
      module procedure :: construct_ISO8601Date
   end interface ISO8601Date

   ! Derived type for communicating date fields internally
   type :: date_fields
      integer :: year_
      integer :: month_
      integer :: day_
      logical :: is_valid_ = .FALSE.
   end type date_fields

   interface date_fields
      module procedure :: construct_date_fields
   end interface date_fields

   ! Derived type used to store fields from ISO 8601 Times
   type :: ISO8601Time
      private
      integer :: hour_
      integer :: minute_
      integer :: second_
      integer :: millisecond_
      ! Timezone is stored as offset from Z timezone in minutes
      ! Currently, only timezone Z offset is used.
      integer :: timezone_offset_ = Z
   contains
      procedure, public :: get_hour
      procedure, public :: get_minute
      procedure, public :: get_second
      procedure, public :: get_millisecond
      procedure, public :: get_timezone_offset
   end type ISO8601Time

   interface ISO8601Time
      module procedure :: construct_ISO8601Time
   end interface ISO8601Time

   ! Derived type for communicating time fields internally
   type :: time_fields
      integer :: hour_
      integer :: minute_
      integer :: second_
      integer :: millisecond_
      integer :: timezone_offset_
      logical :: is_valid_ = .FALSE.
   end type time_fields

   interface time_fields
      module procedure :: construct_time_fields
   end interface time_fields

   ! Derived type used to store fields from ISO 8601 DateTimes
   type :: ISO8601DateTime
      private
      type(ISO8601Date) :: date_
      type(ISO8601Time) :: time_
   contains
      procedure, public :: get_date
      procedure, public :: get_time
      procedure, public :: get_year => get_year_datetime
      procedure, public :: get_month => get_month_datetime
      procedure, public :: get_day => get_day_datetime
      procedure, public :: get_hour => get_hour_datetime
      procedure, public :: get_minute => get_minute_datetime
      procedure, public :: get_second => get_second_datetime
      procedure, public :: get_millisecond => get_millisecond_datetime
      procedure, public :: get_timezone_offset => get_timezone_offset_datetime
   end type ISO8601DateTime

   interface ISO8601DateTime
      module procedure :: construct_ISO8601DateTime
   end interface ISO8601DateTime

   ! Derived type used to store fields from ISO 8601 Durations
   ! Note that ISO 8601 Duration corresponds to ESMF Interval.
   type :: ISO8601Duration
      private
      integer :: years_
      integer :: months_
      integer :: days_
      integer :: hours_
      integer :: minutes_
      integer :: seconds_
   contains
      procedure, public :: get_years
      procedure, public :: get_months
      procedure, public :: get_days
      procedure, public :: get_hours
      procedure, public :: get_minutes
      procedure, public :: get_seconds
   end type ISO8601Duration

   interface ISO8601Duration
      module procedure :: construct_ISO8601Duration
   end interface ISO8601Duration

   ! Derived type used to store fields from ISO 8601 Interval
   ! Note that ISO 8601 Interval is different than ESMF Interval.
   ! This has not been fully implemented
   type :: ISO8601Interval
      private
      type(ISO8601DateTime) :: start_datetime_
      type(ISO8601DateTime) :: end_datetime_
      integer :: repetitions_ = 1
   contains
      procedure, public :: get_start_datetime
      procedure, public :: get_end_datetime
      procedure, public :: get_repetitions
   end type ISO8601Interval

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

   ! Checks if n is in (open) interval: (lower, upper), [not inclusive]
   pure logical function is_between(lower, upper, n)
      integer, intent(in) :: lower
      integer, intent(in) :: upper
      integer, intent(in) :: n
      is_between = n > lower .and. n < upper
   end function is_between

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
   pure function undelimit(string, delimiter) result(undelimited)
      character(len=*), intent(in) :: string
      character, intent(in) :: delimiter
      character(len=len(string)) :: undelimited
      integer :: i
      integer :: j

      undelimited = ''
      j = 0
      do i=1,len(string)
        if(string(i:i) == delimiter) cycle
        j = j + 1
        undelimited(j:j) = string(i:i)
      end do
   end function undelimit

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

   ! Verify that y is a valid year according the ISO 8601 specification
   pure logical function is_valid_year(y)
      integer, intent(in) :: y
! Strict ISO8601 compliance does not allow years before 1583
#ifdef STRICT_ISO8601
      integer, parameter :: LB_YEAR = 1582
#else
      integer, parameter :: LB_YEAR = -1
#endif
      integer, parameter :: UB_YEAR = 10000

      is_valid_year = is_between(LB_YEAR, UB_YEAR, y)
   end function is_valid_year

   ! Verify that m is a valid month number
   pure logical function is_valid_month(m)
      integer, intent(in) :: m
      integer, parameter :: LB_MONTH = 0
      integer, parameter :: UB_MONTH = 13
      is_valid_month = is_between(LB_MONTH, UB_MONTH, m)
   end function is_valid_month

   ! Verify that hour is a valid hour
   pure logical function is_valid_hour(hour)
      integer, intent(in) :: hour
      integer, parameter :: LB_HOUR = -1
      integer, parameter :: UB_HOUR = 24
      is_valid_hour = is_between(LB_HOUR, UB_HOUR, hour)
   end function is_valid_hour

   ! Verify that minute is a valid minute
   pure logical function is_valid_minute(minute)
      integer, intent(in) :: minute
      integer, parameter :: LB_MINUTE = -1
      integer, parameter :: UB_MINUTE = 60
      is_valid_minute = is_between(LB_MINUTE, UB_MINUTE, minute)
   end function is_valid_minute

   ! Verify that second is a valid second
   ! ISO 8601 allows second=60 (for leap seconds)
   ! This function does not check if it is a valid leap second.
   pure logical function is_valid_second(second)
      integer, intent(in) :: second
      integer, parameter :: LB_SECOND = -1
      integer, parameter :: UB_SECOND = 61
      is_valid_second = is_between(LB_SECOND, UB_SECOND, second)
   end function is_valid_second

   ! Verify that millisecond is a valid millisecond values
   ! ISO 8601 allows fractional seconds, but it does not limit the fractional
   ! part to millisecond.
   pure logical function is_valid_millisecond(millisecond)
      integer, intent(in) :: millisecond
      integer, parameter :: LB_MILLISECOND = -1
      integer, parameter :: UB_MILLISECOND = 1000
      is_valid_millisecond = is_between(LB_MILLISECOND, UB_MILLISECOND, millisecond)
   end function is_valid_millisecond

   ! Verify that the timezone offset is valid
   ! Currently, the only valid offset is 0 corresponding to timezone Z.
   pure logical function is_valid_timezone_offset(timezone_offset)
      integer, intent(in) :: timezone_offset
      is_valid_timezone_offset = (timezone_offset == Z)
   end function is_valid_timezone_offset

! END LOW-LEVEL DATE & TIME PROCESSING PROCEDURES


! DATE & TIME VERIFICATION

   ! Verify all the date fields are valid
   pure logical function is_valid_date(date)
      type(date_fields), intent(in) :: date
      integer, parameter :: LB_DAY = 0

      is_valid_date = is_valid_year(date%year_) .and. &
         is_valid_month(date%month_) .and. &
         is_between(LB_DAY, get_month_end(date%year_, date%month_)+1, date%day_)

   end function is_valid_date

   ! Verify all the time fields are valid
   pure logical function is_valid_time(time)
      type(time_fields), intent(in) :: time

      is_valid_time = is_valid_hour(time%hour_) .and. &
         is_valid_minute(time%minute_) .and. &
         is_valid_second(time%second_) .and. &
         is_valid_millisecond(time%millisecond_) .and. &
         is_valid_timezone_offset(time%timezone_offset_)

   end function is_valid_time

! END DATE & TIME VERIFICATION


! STRING PARSERS

   ! parse string representing a timezone offset (absolute value)
   ! return integer offset in minutes, or on error return negative number
   pure function parse_timezone_offset(offset, field_width) result(tzo)
      character(len=*), intent(in) :: offset
      integer, intent(in) :: field_width
      integer :: offset_length
      integer :: tzo
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
   pure function parse_date(datestring) result(fields)
      character(len=*), intent(in) :: datestring
      type(date_fields) :: fields
      integer, parameter :: LENGTH = 8
      character, parameter :: DELIMITER = '-'
      integer, parameter :: YEAR_POSITION = 1
      integer, parameter :: MONTH_POSITION = 5
      integer, parameter :: DAY_POSITION = 7
      character(len=LENGTH) :: undelimited

      ! Eliminate delimiters so that there is one parsing block
      undelimited=undelimit(datestring, DELIMITER)

      if(len(undelimited) == LENGTH) then
         fields%year_ = read_whole_number(undelimited(YEAR_POSITION:MONTH_POSITION-1))
         fields%month_ = read_whole_number(undelimited(MONTH_POSITION:DAY_POSITION-1))
         fields%day_ = read_whole_number(undelimited(DAY_POSITION:LENGTH))
         fields%is_valid_ = is_valid_date(fields)
      else
         fields%is_valid_ = .FALSE.
      end if

   end function parse_date

   ! Parse ISO 8601 Time string into time fields, check if valid time
   ! and set is_valid_ flag
   pure function parse_time(timestring) result(fields)
      character(len=*), intent(in) :: timestring
      type(time_fields) :: fields
      integer, parameter :: LENGTH = 6
      character, parameter :: TIME_PREFIX = 'T' ! debug
      character, parameter :: DELIMITER = ':'
      character, parameter :: DECIMAL_POINT = '.'
      integer, parameter :: FIELDWIDTH = 2
      integer, parameter :: MS_WIDTH = 3
      integer, parameter :: PSTART = 1
      integer, parameter :: HSTART = 1
      integer, parameter :: HSTOP  = 2
      integer, parameter :: MSTART = 3
      integer, parameter :: MSTOP  = 4
      integer, parameter :: SSTART = 5
      integer, parameter :: SSTOP  = 6
      integer, parameter :: MS_START = 7
      integer, parameter :: MS_STOP  = 9
      logical :: has_millisecond
      integer :: pos
      character(len=len(timestring)) :: undelimited
      character :: c
      character(len=LENGTH) :: offset
      integer :: offset_minutes
      integer :: undelimited_length
      integer :: signum

      has_millisecond = .FALSE.
      offset_minutes = INVALID

      fields = time_fields(-1, -1, -1, -1, -1)

      ! Check for mandatory Time prefix
      pos = PSTART
      if(.not. timestring(pos:pos) == 'T') then
         fields%is_valid_ = .FALSE.
         return
      end if

      ! Find timezone portion
      pos = scan(timestring, '-Z+')

      if(.not. pos > 0) then
         fields%is_valid_ = .FALSE.
         return
      end if

      c = timestring(pos:pos)

      ! Check first character of timezone portion
      select case(c)
         case('Z')
            signum = 0
         case('-')
            signum = -1
         case('+')
            signum = +1
         case default
            fields%is_valid_ = .FALSE.
            return
      end select

      ! Set timezone offset
      if(signum == 0) then
         fields%timezone_offset_ = Z
         fields%is_valid_ = pos == len(timestring)
      else
         offset = undelimit(timestring(pos+1:len_trim(timestring)), DELIMITER)
         offset_minutes = parse_timezone_offset(offset, FIELDWIDTH)
         fields%timezone_offset_ = signum * offset_minutes
         fields%is_valid_ = is_whole_number(offset_minutes)
      end if

      if(.not. fields%is_valid_) return

      ! Select portion starting at fields%hour and ending before timezone
      undelimited = adjustl(timestring(PSTART+1:pos-1))

      ! Remove delimiter and decimal point
      undelimited=undelimit(undelimit(undelimited, DELIMITER), DECIMAL_POINT)
      undelimited_length = len_trim(undelimited)

      ! Check length of undelimited string with or without milliseconds
      if(undelimited_length == LENGTH) then
         fields%is_valid_ = .TRUE.
      elseif(undelimited_length == LENGTH+MS_WIDTH) then
         has_millisecond = .TRUE.
         fields%is_valid_ = .TRUE.
      else
         fields%is_valid_ = .FALSE.
      end if

      if(.not. fields%is_valid_) return

      ! Read time fields
      fields%hour_ = read_whole_number(undelimited(HSTART:HSTOP))
      fields%minute_ = read_whole_number(undelimited(MSTART:MSTOP))
      fields%second_ = read_whole_number(undelimited(SSTART:SSTOP))

      if(has_millisecond) then
         fields%millisecond_ = read_whole_number(undelimited(MS_START:MS_STOP))
      else
         fields%millisecond_ = 0
      end if

      fields%is_valid_ = is_valid_time(fields)
   end function parse_time

! END STRING PARSERS


! HIGH-LEVEL CONSTRUCTORS
   function construct_ISO8601Date(isostring, rc) result(date)
      character(len=*), intent(in) :: isostring
      integer, intent(out) :: rc
      type(ISO8601Date) :: date
      type(date_fields) :: fields
      fields = parse_date(trim(adjustl(isostring)))
      if(fields%is_valid_) then
         date%year_ = fields%year_
         date%month_ = fields%month_
         date%day_ = fields%day_
      else
         __FAIL('Invalid ISO 8601 date string')
      end if

      __RETURN(__SUCCESS)

   end function construct_ISO8601Date

   function construct_ISO8601Time(isostring, rc) result(time)
      character(len=*), intent(in) :: isostring
      integer, intent(inout) :: rc
      type(ISO8601Time) :: time
      type(time_fields) :: fields
      fields = parse_time(trim(adjustl(isostring)))
      if(fields%is_valid_) then
         time%hour_ = fields%hour_
         time%minute_ = fields%minute_
         time%second_ = fields%second_
         time%millisecond_ = fields%millisecond_
         time%timezone_offset_ = fields%timezone_offset_
         __RETURN(__SUCCESS)
      else
         __FAIL('Invalid ISO 8601 time string')
      end if

      __RETURN(__SUCCESS)

   end function construct_ISO8601Time

   function construct_ISO8601DateTime(isostring, rc) result(datetime)
      character(len=*), intent(in) :: isostring
      integer, optional, intent(inout) :: rc
      type(ISO8601DateTime) :: datetime
      character, parameter :: DELIMITER = TIME_PREFIX
      integer :: status
      integer :: time_index = 0
      time_index = index(isostring,TIME_PREFIX)
      if(time_index > 0) then
         datetime%date_ = ISO8601Date(isostring(1:time_index-1), __RC)
         datetime%time_ = ISO8601Time(isostring(time_index:len(isostring)), __RC)
         __RETURN(__SUCCESS)
      else
         __FAIL('Invalid ISO 8601 datetime string')
      end if
   end function construct_ISO8601DateTime

   ! This is not working currently.
   ! Construct ISO8601Duration from isostring from imin to imax
   function construct_ISO8601Duration(isostring, imin, imax, rc) result(duration)
      character(len=*), intent(in) :: isostring
      integer, intent(in) :: imin
      integer, intent(in) :: imax
      integer, optional, intent(out) :: rc
      type(ISO8601Duration) :: duration
      integer :: years = -1
      integer :: months = -1
      integer :: days = -1
      integer :: hours = -1
      integer :: minutes = -1
      integer :: seconds = -1
      integer :: istart = -1
      integer :: istop = -1
      logical :: time_found = .FALSE.
      logical :: successful = .FALSE.
      character :: c
      integer :: pos

      ! Check indices and first character is 'P'
      successful = ((imin > 0) .and. (imax <= len(isostring)) .and. &
         (imin <= imax) .and. (isostring(imin:imin) == 'P'))

      pos = imin + 1

      ! This do loop reads a character at a time, digit and nondigit.
      ! A field string consists of digits forming an integer n followed by
      ! a field character. A field character must be preceded by an integer.
      ! A field character indicates that the preceding digit characters
      ! should be processed as values for the corresponding field.
      ! The field characters are:
      !  Y(ear)
      !  M(onth)
      !  D(ay)
      !  H(our)
      !  M(inute)
      !  S(econd)
      ! The date and time portions of the string are delimited by T.
      ! Fields can be omitted, but they must be in order (see above).
      ! Omitted fields are set to 0.  A zero field cannot be specified by a
      ! field character without a preceding integer.
      do while(successful .and. (pos <= imax))
         successful = .FALSE.
         c = isostring(pos:pos)
         if(time_found) then
            ! Once the time is found, M should be processed as M(inute).
            select case(c)
               case('H')
                  ! Verify the field or preceding fields have not been set.
                  ! Then process the preceding digit character as an integer.
                  ! Once processed reset the istart index to start processing
                  ! digits. The same logic applies for each case below.
                  if(hours >= 0 .or. minutes >= 0 .or. &
                     seconds >= 0 .or. istart < 1) cycle
                  hours = read_whole_number_indexed(isostring, istart, istop)
                  if(hours < 0) cycle
                  istart = 0
               case('M')
                  if(minutes >= 0 .or. seconds >= 0 .or. istart < 1) cycle
                  minutes = read_whole_number_indexed(isostring, istart, istop)
                  if(minutes < 0) cycle
                  if(hours < 0) hours = 0
                  istart = 0
               case('S')
                  if(seconds >= 0 .or. istart < 1) cycle
                  seconds = read_whole_number_indexed(isostring, istart, istop)
                  if(seconds < 0) cycle
                  if(hours < 0) hours = 0
                  if(minutes < 0) minutes = 0
                  istart = 0
               case default
                  if(.not. is_digit(c)) cycle
                  if(istart == 0) istart = pos
                  istop = pos
            end select
         else
            ! Until the time is found, M should be processed as M(onth).
            select case(c)
               case('T')
                  time_found = .TRUE.
                  if(years < 0) years = 0
                  if(months < 0) months = 0
                  if(days < 0) days = 0
               case('Y')
                  if(years >= 0 .or. months >= 0 .or. &
                     days >= 0 .or. istart < 1) cycle
                  years = read_whole_number_indexed(isostring, istart, istop)
                  if(years < 0) cycle
                  istart = 0
               case('M')
                  if(months >= 0 .or. days >= 0 .or. istart < 1) cycle
                  months = read_whole_number_indexed(isostring, istart, istop)
                  if(months < 0) cycle
                  if(years < 0) years = 0
                  istart = 0
               case('D')
                  if(days >= 0 .or. istart < 1) cycle
                  days = read_whole_number_indexed(isostring, istart, istop)
                  if(days < 0) cycle
                  if(years < 0) years = 0
                  if(months < 0) months = 0
                  istart = 0
               case default
                  if(.not. is_digit(c)) cycle
                  ! istart == 0 indicates that a new integer is being processed.
                  if(istart == 0) istart = pos
                  ! The istop index should be the current position.
                  istop = pos
            end select
         end if
         successful = .TRUE.
         pos = pos + 1
      end do

      if(successful) then
         duration%years_= years
         duration%months_= months
         duration%days_= days
         duration%hours_= hours
         duration%minutes_= minutes
         duration%seconds_= seconds
         __RETURN(__SUCCESS)
      else
         __FAIL('Invalid ISO 8601 datetime duration string')
      end if
   end function construct_ISO8601Duration

! END HIGH-LEVEL CONSTRUCTORS


! LOW-LEVEL CONSTRUCTORS

   function construct_date_fields(year, month, day) result(fields)
      integer, intent(in) :: year
      integer, intent(in) :: month
      integer, intent(in) :: day
      type(date_fields) :: fields
      fields%year_ = year
      fields%month_ = month
      fields%day_ = day
   end function construct_date_fields

   pure function construct_time_fields(hour, minute, second, millisecond, &
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
   end function construct_time_fields

! END LOW-LEVEL CONSTRUCTORS


! TYPE-BOUND METHODS

! getters & setters

! ISO8601Date

   integer function get_year(self)
      class(ISO8601Date), intent(in) :: self
      get_year = self%year_
   end function get_year

   integer function get_month(self)
      class(ISO8601Date), intent(in) :: self
      get_month = self%month_
   end function get_month

   integer function get_day(self)
      class(ISO8601Date), intent(in) :: self
      get_day = self%day_
   end function get_day

! ISO8601Time
   integer function get_hour(self)
      class(ISO8601Time), intent(in) :: self
      get_hour = self%hour_
   end function get_hour

   integer function get_minute(self)
      class(ISO8601Time), intent(in) :: self
      get_minute = self%minute_
   end function get_minute

   integer function get_second(self)
      class(ISO8601Time), intent(in) :: self
      get_second = self%second_
   end function get_second

   integer function get_millisecond(self)
      class(ISO8601Time), intent(in) :: self
      get_millisecond = self%millisecond_
   end function get_millisecond

   integer function get_timezone_offset(self)
      class(ISO8601Time), intent(in) :: self
      get_timezone_offset = self%timezone_offset_
   end function get_timezone_offset

! ISO8601DateTime
   integer function get_year_datetime(self)
      class(ISO8601DateTime), intent(in) :: self
      get_year_datetime= self%date_%get_year()
   end function get_year_datetime

   integer function get_month_datetime(self)
      class(ISO8601DateTime), intent(in) :: self
      get_month_datetime = self%date_%get_month()
   end function get_month_datetime

   integer function get_day_datetime(self)
      class(ISO8601DateTime), intent(in) :: self
      get_day_datetime = self%date_%get_day()
   end function get_day_datetime

   integer function get_hour_datetime(self)
      class(ISO8601DateTime), intent(in) :: self
      get_hour_datetime = self%time_%get_hour()
   end function get_hour_datetime

   integer function get_minute_datetime(self)
      class(ISO8601DateTime), intent(in) :: self
      get_minute_datetime = self%time_%get_minute()
   end function get_minute_datetime

   integer function get_second_datetime(self)
      class(ISO8601DateTime), intent(in) :: self
      get_second_datetime = self%time_%get_second()
   end function get_second_datetime

   integer function get_millisecond_datetime(self)
      class(ISO8601DateTime), intent(in) :: self
      get_millisecond_datetime = self%time_%get_millisecond()
   end function get_millisecond_datetime

   integer function get_timezone_offset_datetime(self)
      class(ISO8601DateTime), intent(in) :: self
      get_timezone_offset_datetime = self%time_%get_timezone_offset()
   end function get_timezone_offset_datetime

   function get_date(self) result(date)
      class(ISO8601DateTime), intent(in) :: self
      type(ISO8601Date) :: date
      date = self%date_
   end function get_date

   function get_time(self) result(time)
      class(ISO8601DateTime), intent(in) :: self
      type(ISO8601Time) :: time
      time = self%time_
   end function get_time

! ISO8601Duration
   integer function get_years(self)
      class(ISO8601Duration), intent(in) :: self
      get_years = self%years_
   end function get_years

   integer function get_months(self)
      class(ISO8601Duration), intent(in) :: self
      get_months = self%months_
   end function get_months

   integer function get_days(self)
      class(ISO8601Duration), intent(in) :: self
      get_days = self%days_
   end function get_days

   integer function get_hours(self)
      class(ISO8601Duration), intent(in) :: self
      get_hours = self%hours_
   end function get_hours

   integer function get_minutes(self)
      class(ISO8601Duration), intent(in) :: self
      get_minutes = self%minutes_
   end function get_minutes

   integer function get_seconds(self)
      class(ISO8601Duration), intent(in) :: self
      get_seconds = self%seconds_
   end function get_seconds

! ISO8601Interval
   function get_start_datetime(self) result(datetime)
      class(ISO8601Interval), intent(in) :: self
      type(ISO8601DateTime) :: datetime
      datetime = self%start_datetime_
   end function get_start_datetime

   function get_end_datetime(self) result(datetime)
      class(ISO8601Interval), intent(in) :: self
      type(ISO8601DateTime) :: datetime
      datetime = self%end_datetime_
   end function get_end_datetime

   integer function get_repetitions(self)
      class(ISO8601Interval), intent(in) :: self
      get_repetitions = self%repetitions_
   end function get_repetitions

! END TYPE-BOUND METHODS


! HIGH-LEVEL CONVERSION PROCEDURES

   ! Convert ISO 8601 string to packed integer YYYYMMDD
   function convert_ISO8601_to_integer_date(isostring, rc) result(integer_date)
      character(len=*), intent(in) :: isostring
      integer, optional, intent(out) :: rc
      integer :: integer_date
      type(ISO8601Date) :: date
      integer :: status

      date = ISO8601Date(isostring, __RC)

      integer_date = date%get_year()*ID_YEAR + date%get_month()*ID_MONTH + &
         date%get_day()*ID_DAY

      __RETURN(__SUCCESS)
   end function convert_ISO8601_to_integer_date

   ! Convert ISO 8601 string to packed integer HHMMSS
   function convert_ISO8601_to_integer_time(isostring, rc) result(integer_time)
      character(len=*), intent(in) :: isostring
      integer, optional, intent(out) :: rc
      integer :: integer_time
      type(ISO8601Time) :: time
      integer :: status

      time = ISO8601Time(isostring, __RC)

      integer_time = time%get_hour()*IT_HOUR + time%get_minute()*IT_MINUTE + &
         time%get_second()*IT_SECOND

      __RETURN(__SUCCESS)
   end function convert_ISO8601_to_integer_time

! END HIGH-LEVEL CONVERSION PROCEDURES

end module MAPL_ISO8601_DateTime
