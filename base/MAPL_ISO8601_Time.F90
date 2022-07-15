! ISO 8601 Date/Time Handling
! This module implements the ISO 8601 standard for date/time strings.
! Specifically, it implements ISO 8601-1:2019, but not ISO 8601-1:2019-2
! Zero-padded strings must be zero padded to their full width, unless
! otherwise specified.

!      YYYY
! Years are represented by 4 numerical characters (Y).
! The range is: '0000' to '9999' representing the years 1 BCE ('0000') to
! 9999 CE ('9999').
! Strict ISO 8601-1:2019 compliance disallows years before 1583 CE.
! Strict ISO 8601-1:2019 compliance can be enabled with 'STRICT_ISO8601'. 


!      ±YYYYY
! The optional ±YYYYY extension is not supported.

! These date formats are supported.
!      YYYY-MM-DD or YYYYMMDD
!      YYYY-MM (but not YYYYMM)
! YYYY is the year as specified above. 
! MM is the zero-padded month from '01' (January) to '12' (December).
! DD is the zero-padded day of the month from '01' (1) to '31' (31).
! The range of allowed DD strings varies according to the actual
! calendar, though the first day is always '01'.

! These formats are not supported (calendar week and ordinal day).
!      YYYY-Www	or	YYYYWww
!      YYYY-Www-D	or	YYYYWwwD
!      YYYY-DDD	or	YYYYDDD

! Time
!      Thh:mm:ss.sss	or	Thhmmss.sss
!      Thh:mm:ss	or	Thhmmss
!      Thh:mm	or	Thhmm
!      Thh

! Fully-formed time with time zone. Local time not-supported
!      <time>Z
! These time zone formats are not implemented.
!      <time>±hh:mm
!      <time>±hhmm
!      <time>±hh

! Datetime
!      <date>T<time>

! ISO 8601 Durations
!      PnYnMnDTnHnMnS

! These ISO 8601 Duration formats are not supported.
!      PnW
!      P<date>T<time>

! ISO 8601 Interval
!      <start>/<end>
!      <start>/<duration>
!      <duration>/<end>
!      <duration>

! Repeating ISO 8601 Intervals are not supported.
!      Rn/<interval>
!      R/<interval>

!#define STRICT_ISO8601 !Uncomment for strict ISO8601 compliance
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ISO8601_Time
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use ESMF
   implicit none

!   private

   public :: convert_ISO8601_to_integer_time
   public :: convert_ISO8601_to_integer_date
   public :: convert_ISO8601_to_esmf_time
   public :: convert_ISO8601_to_esmf_timeinterval

   interface operator(.divides.)
      module procedure :: divides
   end interface divides

   interface get_integer_digit
      module procedure :: get_integer_digit_from_character
      module procedure :: get_integer_digit_from_string
   end interface get_integer_digit

   ! Error handling
   integer, parameter :: SUCCESS = 0
   integer, parameter :: PARSING_ERROR = -2
   integer, parameter :: INVALID = -1

   ! parameters for processing date, time, and datetime strings
   integer, parameter :: NUM_DATE_FIELDS = 3
   integer, parameter :: NUM_TIME_FIELDS = 5
   character(len=10), parameter :: DIGIT_CHARACTERS = '0123456789'
   character, parameter :: TIME_PREFIX = 'T'
   integer, parameter :: MINUTES_PER_HOUR = 60

   integer, parameter :: Z = 0

   integer, parameter :: ID_WIDTH = 100
   integer, parameter :: ID_DAY = 1
   integer, parameter :: ID_MONTH = ID_WIDTH * ID_DAY
   integer, parameter :: ID_YEAR = ID_WIDTH * ID_MONTH

   integer, parameter :: IT_WIDTH = ID_WIDTH
   integer, parameter :: IT_SECOND = 1
   integer, parameter :: IT_MINUTE = IT_WIDTH * IT_SECOND
   integer, parameter :: IT_HOUR = IT_WIDTH * IT_MINUTE

   type :: ISO8601Date
      private
      integer :: year_
      integer :: month_
      integer :: day_
   contains
      procedure, public :: get_year
      procedure, private :: set_year
      procedure, public :: get_month
      procedure, private :: set_month
      procedure, public :: get_day
      procedure, private :: set_day
   end type ISO8601Date

   interface ISO8601Date
      module procedure :: construct_ISO8601Date
   end interface ISO8601Date

   type :: date_fields
      private
      integer :: year_
      integer :: month_
      integer :: day_
      logical :: is_valid_
   contains
      procedure, public :: get_year
      procedure, private :: set_year
      procedure, public :: get_month
      procedure, private :: set_month
      procedure, public :: get_day
      procedure, private :: set_day
      logical :: is_valid_
   end type date_fields
   
   type :: ISO8601Time
      private
      integer :: hour_
      integer :: minute_
      integer :: second_
      integer :: millisecond_
      integer :: timezone_offset_ = Z
   contains
      procedure, public :: get_hour
      procedure, private :: set_hour
      procedure, public :: get_minute
      procedure, private :: set_minute
      procedure, public :: get_second
      procedure, private :: set_second
      procedure, public :: get_millisecond
      procedure, private :: set_millisecond
      procedure, public :: get_timezone_offset
      procedure, private :: set_timezone_offset
   end type ISO8601Time

   interface ISO8601Time
      module procedure :: construct_ISO8601Time
   end interface ISO8601Time

   type :: time_fields
      private
      integer :: hour_
      integer :: minute_
      integer :: second_
      integer :: millisecond_
      integer :: timezone_offset_
      logical :: is_valid_
   contains
      procedure, public :: get_hour
      procedure, private :: set_hour
      procedure, public :: get_minute
      procedure, private :: set_minute
      procedure, public :: get_second
      procedure, private :: set_second
      procedure, public :: get_millisecond
      procedure, private :: set_millisecond
      procedure, public :: get_timezone_offset
      procedure, private :: set_timezone_offset
      procedure, public :: is_valid
      procedure, private :: set_valid
   end type time_fields

   type :: ISO8601DateTime
      private
      type(ISO8601Date) :: date_
      type(ISO8601Time) :: time_
   contains
      procedure, public :: get_date
      procedure, private :: set_date
      procedure, public :: get_time
      procedure, private :: set_time
      procedure, public :: get_year
      procedure, private :: set_year
      procedure, public :: get_month
      procedure, private :: set_month
      procedure, public :: get_day
      procedure, private :: set_day
      procedure, public :: get_hour
      procedure, private :: set_hour
      procedure, public :: get_minute
      procedure, private :: set_minute
      procedure, public :: get_second
      procedure, private :: set_second
      procedure, public :: get_millisecond
      procedure, private :: set_millisecond
      procedure, public :: get_timezone_offset
      procedure, private :: set_timezone_offset
   end type ISO8601DateTime

   interface ISO8601DateTime
      module procedure :: construct_ISO8601DateTime
   end interface ISO8601DateTime

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
      procedure, private :: set_years
      procedure, public :: get_months
      procedure, private :: set_months
      procedure, public :: get_days
      procedure, private :: set_days
      procedure, public :: get_hours
      procedure, private :: set_hours
      procedure, public :: get_minutes
      procedure, private :: set_minutes
      procedure, public :: get_seconds
      procedure, private :: set_seconds
   end type ISO8601Duration

   interface ISO8601Duration
      module procedure :: construct_ISO8601Duration
   end interface ISO8601Duration

   type :: ISO8601TimeInterval
      private
      type(ISO8601DateTime) :: start_datetime_
      type(ISO8601DateTime) :: end_datetime_
      integer :: repetitions_ = 1
   contains
      procedure, public :: get_start_datetime
      procedure, private :: set_start_datetime
      procedure, public :: get_end_datetime
      procedure, private :: set_end_datetime
      procedure, public :: get_repetitions
      procedure, private :: set_repetitions
   end type ISO8601TimeInterval

contains

   elemental pure logical function is_digit(c)
      character, intent(in) :: c
      is_digit = scan(c, DIGIT_CHARACTERS) > 0
   end function is_digit

   pure logical function is_whole_number(n) 
      integer, intent(in) :: n
      is_valid_whole_number = .not. n < 0
   end function is_whole_number

   pure integer function get_integer_digit_from_character(c)
      character, intent(in) :: c
      get_integer_digit_from_character = scan(c, DIGIT_CHARACTERS) - 1
   end function get_integer_digit_from_character
       
   pure integer function get_integer_digit_from_string(s, i)
      character(len=*), intent(in) :: s
      integer, intent(in) :: i

      if((i>0) .and. (i<len(s)+1) then
         get_integer_digit_from_string = get_integer_digit(s(i:i))
      else
         get_integer_digit_from_string = INVALID
      end if
   end function get_integer_digit_from_string
       
   pure integer function read_whole_number(string)
      character(len=*), intent(in) :: string
      integer, parameter :: BASE=10
      integer :: whole
      integer :: i, j
      integer :: place_value
      place_value = 1
      whole = 0

      j = len_trim(string)
      do i= 1, len_trim(string)
         digit_value = get_whole_number(string, j)
         if(is_whole_number(digit_value)) then
            read_whole_number = read_whole_number + digit_value * place_value
            j = j - 1
            place_value = place_value * BASE
         else
            read_whole_number = digit_value
            exit
         end if
      end do
   end function read_whole_number

   pure function get_digits(string) result(digit_string)
      character(len=*), intent(in) :: string
      character(len=len_trim(string)) :: digit_string
      integer :: i, j
     
      j = 0
      do i=1,len(digit_string) 
         if(is_digit(string(i:i)) then
            j = j + 1
            digit_string(j,j) = string(i:i)
         end
      end do

      digit_string = trim(digit_string)

   end function get_digits

   pure function parse_date(datestring) result(fields)
      character(len=*), intent(in) :: datestring
      type(date_fields) :: fields
      integer, parameter :: LENGTH = 8
      character, parameter :: DELIMITER = '-'
      integer, parameter :: YEAR_POSITION = 1
      integer, parameter :: MONTH_POSITION = 5
      integer, parameter :: DAY_POSITION = 7
      character(len=LENGTH) :: undelimited
      
      undelimited=undelimit(datestring, DELIMITER) 

      if(len(undelimited) == LENGTH) then
         fields%year_ = read_whole_number(undelimited(YEAR_POSITION, MONTH_POSITION-1))
         fields%month_ = read_whole_number(undelimited(MONTH_POSITION, DAY_POSITION-1))
         fields%day_ = read_whole_number(undelimited(DAY_POSITION, LENGTH))
         fields%is_valid_ = is_valid_date(fields)
      else
         fields%is_valid_ = .FALSE.
      end if
 
   end function parse_date

   pure function parse_timezone_offset(offset, field_width) result(tzo)
      character(len=*), intent(in) :: offset
      integer, intent(in) :: field_width
      integer :: offset_length
      integer :: tzo
      integer :: minutes
      integer :: hours
      
      offset_length = len_trim(offset)

      if(offset_length == field_width) then
         hours = read_whole_number(offset)
         tzo = hours*MINUTES_PER_HOUR
      elseif (offset_length == 2*field_width) then
         hours = read_whole_number(offset(1:field_width))
         minutes = read_whole_number(offset(field_width+1:len(offset)))    
         if(is_whole_number(hours) .and. is_whole_number(minutes) then
            tzo = hours*MINUTES_PER_HOUR + minutes
         else
            tzo = INVALID
         end if
      else
         tzo = INVALID
      end if

   end function  parse_timezone_offset
 
   pure function parse_time(timestring) result(fields)
      character(len=*), intent(in) :: timestring
      type(time_fields) :: fields
      integer, parameter :: LENGTH = 6
      character, parameter :: DELIMITER = ':'
      character, parameter :: DECIMAL_POINT = '.' 
      integer, parameter :: FIELDWIDTH = 2
      integer, parameter :: MS_WIDTH = 3
      integer, parameter :: PSTART = 1
      integer, parameter :: PSTOP = PSTART + len(TIME_PREFIX) - 1
      integer, parameter :: HSTART = 1
      integer, parameter :: MSTART = HSTART + FIELDWIDTH
      integer, parameter :: SSTART = HSTART + 2*FIELDWIDTH
      integer, parameter :: MS_START = HSTART + 3*FIELDWIDTH
      integer, parameter :: HSTOP = MSTART - 1
      integer, parameter :: MSTOP = SSTART - 1
      integer, parameter :: SSTOP = MS_START - 1
      integer, parameter :: MS_STOP = MS_START + MS_WIDTH - 1
      logical :: has_millisecond = .FALSE.
      integer :: pos
      character(len=LENGTH) :: undelimited
      character :: c
      character(len=LENGTH) :: offset
      integer :: offset_minutes = -1
      integer :: undelimited_length
      integer :: signum
      
      ! Check for mandatory Time prefix
      pos = PSTART

      if(.not. timestring(pos:pos) == TIME_PREFIX) then
         fields%is_valid_ = .FALSE.
         return
      end if
      
      pos = scan(timestring, '-Z+')

      if(.not. pos > 0) then
         fields%is_valid_ = .FALSE.
         return
      end if

      c = timestring(pos:pos)

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
      end select case

      if(signum == 0) then
         fields%timezone_offset_ = Z
         fields%is_valid_ = pos == len(timestring)
      else
         offset = undelimit(timestring(pos+1:len(timestring)), DELIMITER)
         offset_minutes = parse_timezone_offset(offset,, FIELDWIDTH)
         fields%is_valid_ = is_whole_number(offset_minutes)
         fields%timezone_offset_ = signum * offset_minutes
      end if

      if(.not. fields%is_valid_) return

      ! Select portion starting at fields%hour and ending before timezone
      undelimited = timestring(PSTOP+1:pos-1)

      ! Remove delimiter and decimal point
      undelimited=undelimit(undelimit(undelimited, DELIMITER), DECIMAL_POINT) 
      undelimited_length = len_trim(undelimited)

      if(undelimited_length == LENGTH) then
         fields%is_valid_ = .TRUE.
      elseif(undelimited_length == LENGTH+MS_WIDTH) then
         has_millisecond = .TRUE.
         fields%is_valid_ = .TRUE.
      else
         fields%is_valid_ = .FALSE.
      end if 

      if(.not. fields%is_valid_) return

      fields%hour_ = read_whole_number(undelimited(HSTART, HSTOP))
      fields%minute_ = read_whole_number(undelimited(MSTART,MSTOP))
      fields%second_ = read_whole_number(undelimited(SSTART, SSTOP))

      if(has_millisecond) then
         fields%millisecond_ = read_whole_number(undelimited(MS_START, MS_STOP))
      else
         fields%millisecond_ = 0
      end if

      fields%is_valid_ = is_valid_time(fields)
   end function parse_time


! LOW-LEVEL DATE & TIME PROCESSING UTILITIES
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

   pure logical function is_between(lower, upper, n) 
      integer, intent(in) :: lower
      integer, intent(in) :: upper
      integer, intent(in) :: n
      is_between = n > lower .and. n < upper
   end function is_between

   ! Return true if y is a leap year, false otherwise
   pure logical function is_leap_year(y)
      integer, intent(in) :: y
      ! Leap years are years divisible by 400 or (years divisible by 4 and not divisible by 100)
      is_leap_year = 400 .divides. y .or. ( 4 .divides. y .and. .not. 100 .divides. y)
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
      integer, dimension(:) :: month_ends

      month_ends = get_month_ends(y)    
      get_month_end = month_ends(m)

   end function get_month_end

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

   pure logical function is_valid_month(m)
      integer, intent(in) :: m
      integer, parameter :: LB_MONTH = 0
      integer, parameter :: UB_MONTH = 13
      integer, parameter :: NUM_MONTHS = UB_MONTH - 1
      is_valid_month = is_between(LB_MONTH, UB_MONTH, m)
   end function is_valid_month

   pure logical function is_valid_hour(hour)
      integer, intent(in) :: hour
      integer, parameter :: LB_HOUR = -1
      integer, parameter :: UB_HOUR = 24
      is_valid_hour = is_between(LB_HOUR, UB_HOUR, hour)
   end function is_valid_hour 

   pure logical function is_valid_minute(minute)
      integer, intent(in) :: minute
      integer, parameter :: LB_MINUTE = -1
      integer, parameter :: UB_MINUTE = 60
      is_valid_minute = is_between(LB_MINUTE, UB_MINUTE, minute)
   end function is_valid_minute 

   pure logical function is_valid_second(second)
      integer, intent(in) :: second
      integer, parameter :: LB_SECOND = -1
      integer, parameter :: UB_SECOND = 61
      is_valid_second = is_between(LB_SECOND, UB_SECOND, second)
   end function is_valid_second 

   pure logical function is_valid_millisecond(millisecond)
      integer, intent(in) :: millisecond
      integer, parameter :: LB_MILLISECOND = -1
      integer, parameter :: UB_MILLISECOND = 1000
      is_valid_millisecond = is_between(LB_MILLISECOND, UB_MILLISECOND, millisecond)
   end function is_valid_millisecond 

   pure logical function is_valid_timezone_offset(timezone_offset)
      integer, intent(in) :: timezone_offset
      is_valid_timezone_offset = (timezone_offset == Z)
   end function is_valid_timezone_offset

! END LOW-LEVEL DATE & TIME PROCESSING UTILITIES


! DATE & TIME VERIFICATION

   pure logical function is_valid_date(date)
      type(date_fields), intent(in) :: date
      integer, parameter :: LB_DAY = 0
      integer :: month_end

      is_valid_date = is_valid_year(date%year_) .and. &
         is_valid_month(date%month_) .and. &
         is_between(LB_DAY, get_month_end(date%year_, date%month_)+1, date%day_) 

   end function is_valid_date

   pure logical function is_valid_time(time)
      type(time_fields), intent(in) :: time
      
      is_valid_time = is_valid_hour(time%hour_) .and. &
         is_valid_minute(time%minute_) .and. &
         is_valid_second(time%second_) .and. &
         is_valid_millisecond(time%millisecond_) .and. &
         is_valid_timezone_offset(time%timezone_offset_)

   end function is_valid_time

! END DATE & TIME VERIFICATION


! HIGH-LEVEL CONSTRUCTORS

   function construct_ISO8601Date(isostring, rc) result(date)
      character(len=*), intent(in) :: isostring
      integer, intent(inout) :: rc
      type(ISO8601Date) :: date
      integer :: status
      type(date_fields) :: fields
      fields = parse_date(trim(adjustl(isostring)))
      if(fields%is_valid_) then
         date%year_ = fields%year_
         date%month_ = fields%month_
         date%day_ = fields%day_
      else
         status = PARSING_ERROR
      end if
      rc = status
   end function construct_ISO8601Date

   function construct_ISO8601Time(isostring, rc) result(time)
      character(len=*), intent(in) :: isostring
      integer, intent(inout) :: rc
      type(ISO8601Time) :: time
      integer :: status
      type(time_fields) :: fields
      fields = parse_time(trim(adjustl(isostring)))
      if(fields%is_valid_) then
         date%year_ = fields%year_
         date%month_ = fields%month_
         date%day_ = fields%day_
      else
         status = PARSING_ERROR
      end if
      rc = status
   end function construct_ISO8601Time

   function construct_ISO8601DateTime(isostring, rc) result(datetime)
      character(len=*), intent(in) :: isostring
      integer, intent(inout) :: rc
      type(ISO8601DateTime) :: datetime
      integer :: status
      character, parameter :: DELIMITER = TIME_PREFIX 
      type(ISO8601Date) :: date
      type(ISO8601Time) :: time
      integer :: time_index = 0
      time_index = index(isostring,TIME_PREFIX)
      if(time_index > 0) then
         date=ISO8601Date(isostring(1:time_index-1), _RC)
         time=ISO8601Time(isostring(time_index:len(isostring)), __RC__)
      else
         status = PARSING_ERROR
      end if

      rc = status
   end function construct_ISO8601DateTime

   function construct_ISO8601Duration(isostring, rc) result(duration)
      character(len=*), intent(in) :: isostring
      integer, intent(inout) :: rc
      type(ISO8601Duration) :: duration
      integer, parameter :: PREFIX_POSITION = 1
      character, parameter :: DURATION_PREFIX = 'P'
      character, parameter :: YEAR_DESIGNATOR = 'Y'
      character, parameter :: MONTH_DESIGNATOR = 'M'
      character, parameter :: DAY_DESIGNATOR = 'D'
      character, parameter :: HOUR_DESIGNATOR = 'H'
      character, parameter :: MINUTE_DESIGNATOR = 'M'
      character, parameter :: SECOND_DESIGNATOR = 'S'
     !PnYnMnDTnHnMnS
      character(len=:), allocatable :: ljustified
      logical :: is_duration
      integer :: date_pos
      integer :: time_pos
      integer :: year_pos
      integer :: month_pos
      integer :: day_pos
      integer :: hour_pos
      integer :: minute_pos
      integer :: second_pos
      integer :: year
      integer :: month
      integer :: day
      integer :: hour
      integer :: minute
      integer :: second
      integer :: status
 
      ljustified = adjustl(isostring)
      if(ljustified(PREFIX_POSITION:PREFIX_POSITION) == DURATION_PREFIX) then
         ! Write function to parse based on designator & preceding digits
         ! Return whole_number, 0 if not present
         
         date_pos = PREFIX_POSITION+1
         time_pos = index(ljustified, TIME_PREFIX)
         if(day_pos < time_pos) then
            ! Date from day_pos to time_pos-1  
            ! Date and Time
            year_pos = index(ljustified(date_pos:time_pos-1), YEAR_DESIGNATOR)
            month_pos = index(ljustified(date_pos:time_pos-1), YEAR_DESIGNATOR)
            day_pos = index(ljustified(date_pos:time_pos-1), YEAR_DESIGNATOR)
            if((year_pos <= month_pos) .and. (month_pos <= day_pos)) then
               if(year_pos > 0) then
                  year = 
               end if
            end if
         end if
      else
         status = INVALID
      end if
      
      rc = status
   end function construct_ISO8601Duration

   pure integer function parse_duration_field(duration, specifier)
      character(len=*), intent(in) :: duration
   end function parse_duration_field
   
   function construct_ISO8601Interval(isostring, rc) result(interval)
      character(len=*), intent(in) :: isostring
      integer, intent(inout) :: rc
      type(ISO8601Interval) :: interval
      integer :: status
      RC = INVALID
   end function construct_ISO8601Interval

! END HIGH-LEVEL CONSTRUCTORS


! TYPE-BOUND METHODS
   integer function year(self)
      class(ISO8601DateTime), intent(in) :: self
      year = self%date%year_
   end function year

   integer function month(self)
      class(ISO8601DateTime), intent(in) :: self
      month = self%date%month_
   end function month

   integer function day(self)
      class(ISO8601DateTime), intent(in) :: self
      integer :: d
      day = self%date%day_
   end function day

   integer function hour(self)
      class(ISO8601DateTime), intent(in) :: self
      hour = self%date%hour_
   end function hour

   integer function minute(self)
      class(ISO8601DateTime), intent(in) :: self
      minute = self%date%minute_
   end function minute

   integer function second(self)
      class(ISO8601DateTime), intent(in) :: self
      second = self%date%second_
   end function second

   integer function millisecond(self)
      class(ISO8601DateTime), intent(in) :: self
      millisecond = self%date%millisecond_
   end function millisecond

   integer function timezone_offset(self)
      class(ISO8601DateTime), intent(in) :: self
      timezone_offset = self%date%timezone_offset_
   end function timezone_offset

! END TYPE-BOUND METHODS

! HIGH-LEVEL CONVERSION FUNCTIONS
   
   function convert_ISO8601_to_esmf_time(isostring, unusable, rc) result(time)
      character(len=*), intent(in) :: isostring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(ESMF_Time) :: time
      integer :: status
 
      type(ISO8601DateTime) :: datetime
      
      datetime = ISO8601DateTime(isostring, __RC__)

      call ESMF_TimeSet(time,yy = datetime%year(), mm = datetime%month(), &
         dd = datetime%day(), h = datetime%hour(), m = datetime%minute(), &
         s= datetime & second(), __RC__)

      status = SUCCESS
      if(present(rc)) rc = status
      _UNUSED_DUMMY(unusable)

   end function convert_ISO8601_to_esmf_time

   function convert_ISO8601_to_esmf_timeinterval(isostring, unusable, rc) result(interval)
      character(len=*), intent(in) :: isostring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(ESMF_TimeInterval) :: interval
      integer :: status
 
      type(ISO8601Duration) :: duration
      
      duration = ISO8601Duration(isostring, __RC__)

      call ESMF_TimeIntervalSet(interval, yy=duration%years, &
         mm=duration%months, d=duration%days, h=duration%hours, &
         m=duration%mins, s=duration%secs, __RC__)

      status = SUCCESS
      if(present(rc)) rc = status
      _UNUSED_DUMMY(unusable)
   end function convert_ISO8601_to_esmf_timeinterval

   function convert_ISO8601_to_integer_date(string, unusable, rc) result(integer_date)
      character(len=*), intent(in) :: isostring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: integer_date
      integer :: status

      type(ISO8601Date) :: date
      
      date = ISO8601Date(isostring, __RC__)

      integer_date = date%year()*ID_YEAR + date%month()*ID_MONTH + date%day()*ID_DAY
      
      status = SUCCESS
      if(present(rc)) rc = status
      _UNUSED_DUMMY(unusable)

   end function convert_ISO8601_to_integer_date

   function convert_ISO8601_to_integer_time(isostring, unusable, rc) result(integer_time)
      character(len=*), intent(in) :: isostring
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: integer_time
      integer :: status

      type(ISO8601Time) :: time
      
      time = ISO8601Time(isostring, __RC__)

      integer_time = time%hour()*ID_YEAR + time%minute()*ID_MINUTE + time%second()*ID_SECOND
      
      status = SUCCESS
      if(present(rc)) rc = status
      _UNUSED_DUMMY(unusable)

   end function convert_ISO8601_to_integer_time

! END HIGH-LEVEL CONVERSION FUNCTIONS
end module MAPL_ISO8601_Time
