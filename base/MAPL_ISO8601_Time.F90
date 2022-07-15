!Allow ordinal dates as input/output?

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
   !public :: convert_ISO8601_to_esmf_timeinterval

   interface operator(.divides.)
      module procedure :: divides
   end interface divides

   ! Error handling
   integer, parameter :: SUCCESS = 0
   integer, parameter :: PARSING_ERROR = -2

   ! parameters for processing date, time, and datetime strings
   integer, parameter :: NUM_DATE_FIELDS = 3
   integer, parameter :: NUM_TIME_FIELDS = 5
   character(len=10), parameter :: WHOLE_DIGITS = '0123456789'
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
      integer :: year
      integer :: month
      integer :: day
   end type ISO8601Date

   interface ISO8601Date
      module procedure :: construct_ISO8601Date
   end interface ISO8601Date

   type :: date_fields
      integer :: year
      integer :: month
      integer :: day
      logical :: is_valid
   end type date_fields
   
   type :: ISO8601Time
      integer :: hour
      integer :: minute
      integer :: second
      integer :: millisecond
      integer :: timezone_offset = Z
   end type ISO8601Time

   interface ISO8601Time
      module procedure :: construct_ISO8601Time
   end interface ISO8601Time

   type :: time_fields
      integer :: hour
      integer :: minute
      integer :: second
      integer :: millisecond
      integer :: timezone_offset 
      logical :: is_valid
   end type time_fields

   type :: ISO8601DateTime
      type(ISO8601Date) :: date
      type(ISO8601Time) :: time
   contains
      procedure, pass(self) :: year
      procedure, pass(self) :: month
      procedure, pass(self) :: day
      procedure, pass(self) :: hour
      procedure, pass(self) :: minute
      procedure, pass(self) :: second
      procedure, pass(self) :: millisecond
      procedure, pass(self) :: timezone_offset
   end type ISO8601DateTime

   interface ISO8601DateTime
      module procedure :: construct_ISO8601DateTime
   end interface ISO8601DateTime

   type :: ISO8601Duration
      integer :: years
      integer :: months
      integer :: days
      integer :: hours
      integer :: minutes
      integer :: seconds
   end type ISO8601Duration

   interface ISO8601Duration
      module procedure :: construct_ISO8601Duration
   end interface ISO8601Duration

   type :: ISO8601TimeInterval
      type(ISO8601DateTime) :: start_datetime
      type(ISO8601DateTime) :: end_datetime
      integer :: repetitions = 1
   end type ISO8601TimeInterval

contains

   pure logical function is_whole_number(n) 
      integer, intent(in) :: n
      is_valid_whole_number = .not. n < 0
   end function is_whole_number

   pure integer function get_whole_number_from_character(c)
      character, intent(in) :: c
      get_whole_number_from_character = scan(c, WHOLE_DIGITS) - 1
   end function get_whole_number_from_character
       
   pure integer function get_whole_number(s, i)
      character(len=*), intent(in) :: s
      integer, intent(in) :: i

      if((i>0) .and. (i<len(s)+1) then
         get_whole_number = get_whole_number_from_character(s(i:i))
      else
         get_whole_number=-1
      end if
   end function get_whole_number
       
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
            break
         end if
      end do
   end function read_whole_number

   pure function parse_date(datestring) result(fields)
      character(len=*), intent(in) :: datestring
      type(date_fields) :: fields
      integer, parameter :: LENGTH = 8
      character, parameter :: DELIMITER = '-'
      integer, parameter :: YEAR_POSITION = 1
      integer, parameter :: MONTH_POSITION = 5
      integer, parameter :: DAY_POSITION = 7
      character(len=LENGTH) :: undelimited
      integer :: year => fields%year
      integer :: month => fields%month
      integer :: day => fields%day
      logical :: is_valid => fields%is_valid
      
      undelimited=undelimit(datestring, DELIMITER) 

      is_valid=len(undelimited) == LENGTH
      if(.not. is_valid) return
      
      year = read_whole_number(undelimited(YEAR_POSITION, MONTH_POSITION-1))
      month = read_whole_number(undelimited(MONTH_POSITION, DAY_POSITION-1))
      day = read_whole_number(undelimited(DAY_POSITION, LENGTH))
      is_valid = is_valid_date(fields)
 
   end function parse_date

   pure function  parse_timezone_offset(offset, field_width) result(tzo)
      character(len=*), intent(in) :: offset
      integer, intent(in) :: field_width
      integer, parameter :: INVALID_OFFSET = -1
      integer :: tzo
      integer :: minutes
      integer :: hours

      select case(len_trim(offset))
         case(field_width)
            hours = read_whole_number(offset)
            tzo = hours*MINUTES_PER_HOUR

         case(2*field_width)
            hours = read_whole_number(offset(1:field_width))
            minutes = read_whole_number(offset(field_width+1:len(offset)))    
            if(is_whole_number(hours) .and. is_whole_number(minutes) then
               tzo = hours*MINUTES_PER_HOUR + minutes
            else
               tzo = INVALID_OFFSET
            end if

         case default
            tzo = INVALID_OFFSET

      end select case

   end function  parse_timezone_offset
 
   pure integer function plus_or_minus(c)
      character, intent(in) :: c
      if(c == '-') then
         plus_or_minus = -1
      else
         plus_or_minus = +1
      end if
   end function multiply_by

   pure function parse_time(timestring) result(fields)
      character(len=*), intent(in) :: timestring
      type(time_fields) :: fields
      integer, parameter :: LENGTH = 6
      character, parameter :: DELIMITER = ':'
      character, parameter :: DECIMAL_POINT = '.' 
      character(len=3), parameter :: TIMEZONE_PREFIXES = 'Z+-'
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
      integer :: hour => fields%hour
      integer :: minute => fields%minute
      integer :: second => fields%second
      integer :: millisecond => fields%millisecond
      integer :: timezone_offset => fields%timezone_offset
      logical :: is_valid => fields%is_valid
      character(len=LENGTH) :: undelimited
      character :: c
      integer :: multiplier
      character(len=LENGTH) :: offset
      integer :: offset_minutes = -1
      
      ! Check for mandatory Time prefix
      pos = PSTART
      is_valid = timestring(pos:pos) == TIME_PREFIX
      if(.not. is_valid) return
      
      ! Parse mandatory timezone offset
      pos = scan(timestring, TIMEZONE_PREFIXES)
      is_valid = pos > 0
      if(.not. is_valid) return

      c = timestring(pos:pos)

      if(c == 'Z') then
         timezone_offset = Z
         is_valid = pos == len(timestring)
      else
         multiplier = plus_or_minus(c)
         offset = undelimit(timestring(pos+1:len(timestring)), DELIMITER)
         offset_minutes = parse_timezone_offset(offset,, FIELDWIDTH)
         is_valid = is_whole_number(offset_minutes)
         if(.not. is_valid) return
         timezone_offset = multiplier * offset_minutes
      end if

      ! Select portion starting at hour and ending before timezone
      undelimited=timestring(PSTOP+1:pos-1)

      ! Remove delimiter and decimal point
      undelimited=undelimit(undelimit(undelimited, DELIMITER), DECIMAL_POINT) 

      select case(len_trim(undelimited))
         case(LENGTH)
            is_valid == .TRUE.
         case(LENGTH+MS_WIDTH)
            has_millisecond = .TRUE.
            is_valid == .TRUE.
         case default
            is_valid = .FALSE.
      end select case

      if(.not. is_valid) return

      hour = read_whole_number(undelimited(HSTART, HSTOP))
      minute = read_whole_number(undelimited(MSTART,MSTOP))
      second = read_whole_number(undelimited(SSTART, SSTOP))

      if(has_millisecond) then
         millisecond = read_whole_number(undelimited(MS_START, MS_STOP))
      else
         millisecond = 0
      end if

      is_valid = is_valid_time(fields)
   end function parse_time

   pure function parse_duration(durationstring) result(duration)
      character(len=*), intent(in) :: durationstring
      type(ISO8601Duration) :: duration

   end function parse_duration
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
      integer :: year => date%year
      integer :: month => date%month
      integer :: day => date%day

      is_valid_date = is_valid_year(year) .and. &
         is_valid_month(month) .and. &
         is_between(LB_DAY, get_month_end(year, month)+1, day) 

   end function is_valid_date

   pure logical function is_valid_time(time)
      integer :: hour => time%hour
      integer :: minute => time%minute
      integer :: second => time%second
      integer :: millisecond => time%millisecond
      integer :: timezone_offset => time%timezone_offset
      
      is_valid_time = is_valid_hour(hour) .and. &
         is_valid_minute(minute) .and. &
         is_valid_second(second) .and. &
         is_valid_millisecond(millisecond) .and. &
         is_valid_timezone_offset(timezone_offset)

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
      if(fields%is_valid) then
         date%year = fields%year
         date%month = fields%month
         date%day = fields%day
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
      if(fields%is_valid) then
         date%year = fields%year
         date%month = fields%month
         date%day = fields%day
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
         date=ISO8601Date(isostring(1:time_index-1), __RC__)
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
      
      character(len=*) :: trimmed
      logical :: is_duration
      integer :: year_pos
      integer :: month_pos
      integer :: day_pos
      integer :: hour_pos
      integer :: minute_pos
      integer :: second_pos
      integer :: status
 
      trimmed = trim(adjustl(isostring))
      if(trimmed(PREFIX_POSITION:PREFIX_POSITION) == DURATION_PREFIX) then
         ! Write function to parse based on designator & preceding digits
         ! Return whole_number, 0 if not present
      else
         status = INVALID_DURATION
      end if
      
      rc = status
   end function construct_ISO8601Duration
   
   function construct_ISO8601Interval(isostring, rc) result(interval)
      character(len=*), intent(in) :: isostring
      integer, intent(inout) :: rc
      type(ISO8601Interval) :: interval
      integer :: status
   end function construct_ISO8601Interval

! END HIGH-LEVEL CONSTRUCTORS


! TYPE-BOUND METHODS
   integer function year(self)
      class(ISO8601DateTime), intent(in) :: self
      year = self%date%year()
   end function year

   integer function month(self)
      class(ISO8601DateTime), intent(in) :: self
      month = self%date%month()
   end function month

   integer function day(self)
      class(ISO8601DateTime), intent(in) :: self
      integer :: d
      day = self%date%day()
   end function day

   integer function hour(self)
      class(ISO8601DateTime), intent(in) :: self
      hour = self%date%hour()
   end function hour

   integer function minute(self)
      class(ISO8601DateTime), intent(in) :: self
      minute = self%date%minute()
   end function minute

   integer function second(self)
      class(ISO8601DateTime), intent(in) :: self
      second = self%date%second()
   end function second

   integer function millisecond(self)
      class(ISO8601DateTime), intent(in) :: self
      millisecond = self%date%millisecond()
   end function millisecond

   integer function timezone_offset(self)
      class(ISO8601DateTime), intent(in) :: self
      timezone_offset = self%date%timezone_offset()
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

! UNUSED
!! OPERATORS
!   public :: operator(==)
!
!   interface operator(==)
!      module procedure :: datefields_equal
!      module procedure :: timefields_equal
!      module procedure :: iso_date_equal
!      module procedure :: iso_time_equal
!      module procedure :: iso_datetime_equal
!   end interface

!   pure function datefields_equal(datefields_a, datefields_b) result(truth_value)
!      type(date_fields), intent(in) :: datefields_a
!      type(date_fields), intent(in) :: datefields_b
!      logical :: truth_value
!      truth_value = (datefields_a%year == datefields_b%year) &
!         .and. (datefields_a%month == datefields_b%month) &
!         .and. (datefields_a%day == datefields_b%day)
!   end function datefields_equal
!   
!   pure function timefields_equal(timefields_a, timefields_b) result(truth_value)
!      type(time_fields), intent(in) :: timefields_a
!      type(time_fields), intent(in) :: timefields_b
!      logical :: truth_value
!      truth_value = (timefields_a%hour == timefields_b%hour) &
!         .and. (timefields_a%minute == timefields_b%minute) &
!         .and. (timefields_a%second == timefields_b%second) &
!         .and. (timefields_a%millisecond == timefields_b%millisecond)
!   end function timefields_equal
!
!   pure function iso_date_equal(date_a, date_b) result(truth_value)
!      type(ISO8601Date), intent(in) :: date_a
!      type(ISO8601Date), intent(in) :: date_b
!      logical :: truth_value
!      truth_value = datefields_equal(date_a%fields, date_b%fields)
!   end function iso_date_equal
!
!   pure function iso_time_equal(time_a, time_b) result(truth_value)
!      type(ISO8601Date), intent(in) :: time_a
!      type(ISO8601Date), intent(in) :: time_b
!      logical :: truth_value
!      truth_value = timefields_equal(time_a%fields, time_b%fields) &
!         .and. time_a%timezone_offset == time_b%timezone_offset
!   end function iso_time_equal
!
!   pure function iso_datetime_equal(datetime_a, datetime_b) result(truth_value)
!      type(ISO8601DateTime), intent(in) :: datetime_a
!      type(ISO8601DateTime), intent(in) :: datetime_b
!      logical :: truth_value
!      truth_value = iso_date_equal(datetime_a%date, datetime_b%date) &
!         .and. iso_time_equal(datetime_a%time, datetime_b%time)
!   end function iso_datetime_equal
!
!! END OPERATORS


   ! <time>Z <timezone_offset>
   ! <time>+-hh <timezone_offset>
   ! <time>+-hh:mm <timezone_offset>
   ! <time>+-hhmm <timezone_offset>

!   function year(date) result(y)
!      class(ISO8601Date), intent(in) :: date
!      integer :: y
!      y = date%year
!   end function year
!
!   function month(date) result(m)
!      class(ISO8601Date), intent(in) :: date
!      integer :: m
!      m = date%month
!   end function month
!
!   function day(date) result(d)
!      class(ISO8601Date), intent(in) :: date
!      integer :: d
!      d = date%day
!   end function day
!
!   function hour(time) result(h)
!      class(ISO8601Time), intent(in) :: time
!      integer :: h
!      h = time%hour
!   end function hour
!
!   function minute(time) result(m)
!      class(ISO8601Time), intent(in) :: time
!      integer :: m
!      m = time%minute
!   end function minute
!
!   function second(time) result(s)
!      class(ISO8601Time), intent(in) :: time
!      integer :: s
!      s = time%second
!   end function second
!
!   function millisecond(time) result(ms)
!      class(ISO8601Time), intent(in) :: time
!      integer :: ms
!      ms = time%millisecond
!   end function millisecond
!
!   function timezone_offset(time) result(tz)
!      class(ISO8601Time), intent(in) :: time
!      integer :: tz
!      tz = time%timezone_offset
!   end function timezone_offset

!   subroutine parse_datestring(s, fields, status)
!      character(len=*), intent(in) :: s
!      integer, dimension(DIM_DF), intent(out) :: fields
!      integer, intent(inout) :: status
!      integer, dimension(DIM_DF) :: f
!      if(is_extended_date(s)) then
!         call parse_isostring(s, fields, FDD, status)
!      else if(is_basic_date(s)) then
!         call parse_isostring(s, fields, FUD, status)
!      else
!         status = -1
!      end if
!   end subroutine parse_datestring
!
!   subroutine parse_timestring(s, fields, status)
!      character(len=*), intent(in) :: s
!      integer, dimension(DIM_TF), intent(out) :: fields
!      integer, intent(inout) :: status
!
!      fields(size(fields)) = TIMEZONE_DEFAULT
!
!      if(is_extended_time(s)) then
!         call parse_isostring(s, fields(1:4), FDT, status)
!      else if(is_basic_time(s)) then
!         call parse_isostring(s, fields(1:4), FUT, status)
!      else
!         status = -1
!      end if
!   end subroutine parse_timestring
!
!   subroutine parse_datetimestring(s, datefields, timefields, status)
!      character(len=*), intent(in) :: s
!      integer, dimension(DIM_DF), intent(out) :: datefields
!      integer, dimension(DIM_TF), intent(out) :: timefields
!      integer, intent(inout) :: status
!      character(len=len(s)), dimension(2) :: parts 
! 
!      parts = split_datetime_string(s) 
!      call parse_datestring(trim(parts(1)),datefields,status)
!      if(status == SUCCESS) then
!         call parse_timestring(trim(parts(2)),timefields,status)
!      else
!         status = -1
!      endif
!   end subroutine parse_datetimestring
!   subroutine parse_isostring(s, fields, fmts, status)
!      character(len=*), intent(in) :: s
!      integer, dimension(:), intent(inout) :: fields
!      character(len=*), intent(in) :: fmts
!      integer, intent(inout) :: status
!      read (s, fmt=fmts, iostat=status) fields
!   end subroutine parse_isostring
!   pure logical function is_valid_day_ints(y, m, d)
!      integer, intent(in) :: y
!      integer, intent(in) :: m
!      integer, intent(in) :: d
!      integer :: month_end
!
!      if(is_valid_year(y) .and. is_valid_month(m)) then
!         month_end = get_month_end(y, m)
!         is_valid_day_ints = is_between(LB_DAY, month_end+1, d) 
!      else
!         is_valid_day_ints = .FALSE.
!      endif
!   end function is_valid_day_ints
! pure function substring(string, n) result(substr)
!    character(len=*), intent(in) :: string
!    integer, intent(in) :: n
!    if(abs(n) > len(string) .or. n == 0) then
!       substr = ''
!    else
!       if(n < 0) then
!          substr = string(len(string) + n + 1, len(string))
!       else
!          substr = string(1, n)
!    else
! end function substring

!   pure function split_to_ints(intstr, widths) result(ints)
!      character(len=*), intent(in) :: intstr
!      integer, dimension(:), intent(in) :: widths
!      integer, dimension(size(widths)) :: ints 
!      integer :: i
!      integer :: first
!      do i, len(widths)
!         read (intstr(first:first+widths(i)-1)), ints(i)
!         first = first + widths(i)
!      end do
!   end function split_to_integer

!! SIMPLE CONSTRUCTORS
!
!   pure function construct_date_fields(fields) result(df)
!      integer, dimension(NUM_DATE_FIELDS), intent(in) :: fields
!      type(date_fields) :: df
!      df%year = fields(YEAR_INDEX)
!      df%month = fields(MONTH_INDEX)
!      df%day = fields(DAY_INDEX)
!   end function construct_date_fields
!
!   pure function construct_time_fields(fields) result(tf)
!      integer, dimension(NUM_TIME_FIELDS), intent(in) :: fields
!      type(time_fields) :: tf
!      tf%hour = fields(HOUR_INDEX)
!      tf%minute = fields(MINUTE_INDEX)
!      tf%second = fields(SECOND_INDEX)
!      tf%millisecond = fields(MILLISECOND_INDEX)
!      tf%timezone_offset = fields(TIMEZONE_INDEX)
!   end function construct_time_fields
!
!! END SIMPLE CONSTRUCTORS

!   pure function character_array(s) result(a)
!      character(len=*), intent(in) :: s
!      character, dimension(len(s)) :: a
!      integer :: i
!      a = [(s(i:i), i= 1, len(s))]
!   end function character_array

!pure function make_string(a) result(s)
!   character, dimension(:), intent(in) :: a 
!   character(len=size(a)) :: s
!   integer :: i
!   do i, len(s), s(i:i) = a(i)
!end function make_string

!   pure function undelimit(string, delimiter) result(undelimited)
!      character(len=*), intent(in) :: string
!      character, intent(in) :: delimiter
!      character(len=*) :: undelimited = ''
!      integer :: i
!      integer :: pos = 1
!      do i = 1, len(string)
!         if(string(i:i) /= d) then
!            undelimited(pos:pos) = string(i:i)
!            pos = pos + 1
!         end if
!      end do
!   end function undelimit
!
!   elemental pure logical function is_digit(c)
!      character, intent(in) :: c
!      integer, parameter :: lb = iachar('0') - 1
!      integer, parameter :: ub = iachar('9') + 1
!      is_digit = iachar(c) > lb .and. iachar(c) < ub
!   end function is_digit
!
!   pure logical function are_digits(s)
!      character(len=*), intent(in) :: s
!      are_digits = len(s) > 0 .and. all(is_digit(make_char_array(s)))
!   end function are_digits
!
!   pure logical function is_extended_date(s)
!      character(len=*), intent(in) :: s
!      is_extended_date = len(s)==10 .and. are_digits(s(1:4)) &
!         .and. s(5:5)==DD .and. are_digits(s(6:7)) .and. s(8:8)==DD &
!         .and. are_digits(s(9:10))
!   end function
!   
!   pure logical function is_basic_date(s)
!      character(len=*), intent(in) :: s
!      is_basic_date = len(s)==8 .and. are_digits(s)
!   end function is_basic_date
!
!   pure logical function is_extended_time(s)
!      character(len=*), intent(in) :: s
!      
!      select case(len(s))
!         case(10)
!            is_extended_time = s(1:1)==TP .and. are_digits(s(2:3)) &
!               .and. s(4:4)==TD .and. are_digits(s(5:6)) .and. s(7:7)==TD &
!               .and. are_digits(s(8:9)) .and. s(10:10)==TZ
!         case(14)
!            is_extended_time = s(1:1)==TP .and. are_digits(s(2:3)) &
!               .and. s(4:4)==TD .and. are_digits(s(5:6)) .and. s(7:7)==TD &
!               .and. are_digits(s(8:9)) .and. s(10:10)=='.' &
!               .and. are_digits(s(11:13)) .and. s(14:14)==TZ
!         case default
!            is_extended_time = .FALSE.
!      end select
!
!   end function is_extended_time
!
!   pure logical function is_basic_time(s)
!      character(len=*), intent(in) :: s
!      
!      select case(len(s))
!         case(8)
!            is_basic_time = s(1:1)==TP .and. are_digits(s(2:7)) &
!               .and. s(8:8)==TZ
!         case(12)
!            is_basic_time = s(1:1)==TP .and. are_digits(s(2:7)) &
!               .and. s(8:8)=='.' .and. are_digits(s(9:11)) .and. s(12:12)==TZ
!         case default
!            is_basic_time = .FALSE.
!      end select
!
!   end function is_basic_time
!   pure function split_datetime_string(s) result(parts)
!      character(len=*), intent(in) :: s
!      character(len=len(s)), dimension(2) :: parts
!      parts = split(s, TP)
!   end function split_datetime_string
!
!   pure function split(s, delimiter) result(parts)
!      character(len=*), intent(in) :: s
!      character, intent(in) :: delimiter
!      integer :: i = -1
!      i = index(s, delimiter)
!      if(i > 0) then
!         parts(1) = adjustl(s(1:i-1))
!         parts(2) = adjustl(s(i:len(s)))
!      else
!         parts(1) = adjustl(s)
!         parts(2) = ''
!      endif
!   end function split
!
! HIGH-LEVEL PROCESSORS

   ! parse date_string to create a ISO8601Date
   ! ISO 8601 defines several date formats. This function parses these formats:
   !  YYYYMMDD
   !  YYYY-MM-DD
   ! Subfields must be 0-padded strings of whole number digits.
   ! YYYY   : interval [0000, 9999] 
   ! MM     : interval [01, 12]
   ! DD     : interval [01, 31]
   ! Support for this format is not supported:
   !  YYYY-MM
   ! More or less than 4 digits is not supported. + or - are not supported.
!   function process_date(date_string) result(date)
!      character(len=*), intent(in) :: date_string
!      type(ISO8601Date) :: date
!      type(date_fields) :: df
!      integer, dimension(DIM_DF) :: fields
!      integer :: status = -1
!      
!      call parse_datestring(date_string, fields, status)
!      if(status == SUCCESS) then
!         df = date_fields(fields)
!         if(df%valid) then
!            date = ISO8601Date(df)
!         else
!            status == -1
!         end if
!      end if
!   end function process_date

   ! parse time_string to create a ISO8601Time
   ! ISO 8601 defines several time formats. This function parses these formats:
   !  THHMMSS.sssZ
   !  THH:MM:SS.sssZ
   ! Subfields must be 0-padded strings of whole number digits.
   ! HH  : interval [00, 23] 
   ! MM  : interval [00, 59]
   ! SS  : interval [00, 59]
   ! sss : interval [000, 999]
   ! Support for this format is not supported:
!   function process_time(time_string) result(time)
!      character(len=*), intent(in) :: time_string
!      type(ISO8601Date) :: time
!      type(time_fields) :: tf
!      integer, dimension(DIM_TF) :: fields
!      integer :: status = -1
!
!      call parse_timestring(time_string, fields, status)
!      if(status == SUCCESS) then
!         tf = time_fields(fields)
!         if(tf%valid) then
!            time = ISO8601Time(df)
!         else
!            status = -1
!         end if
!      end if
!   end function process_time

   ! parse datetime_string to create a ISO8601DateTime
   ! ISO 8601 defines several datetime formats. This function parses these formats:
   !  YYYYMMDDTHHMMSS.sssZ
   !  YYYY-MM-DDTHH:MM:SS.sssZ
   ! See process_date and process_time for more detail on strings
!   function process_datetime(datetime_string) result(datetime)
!      character(len=*), intent(in) :: datetime_string
!      integer, dimension(DIM_DF) :: datefields
!      integer, dimension(DIM_TF) :: timefields
!      type(ISO8601DateTime) :: datetime
!      integer :: status 
!       
!      call parse_datetimestring(datetime_string, datefields, timefields, status)
!      if(status==SUCCESS) then
!         df = date_fields(datefields)
!         tf = time_fields(timefields)
!         if((df%valid) .and. (tf%valid)) then
!            datetime = ISO8601DateTime(ISO8601Date(df), ISO8601Time(tf))
!         else
!            status == -1
!         end if
!      end if
!   end function process_datetime
