!Allow 60 for seconds
!No fractions other than milliseconds (explicit)
!No weeks
!Z required for time
!No parts of day !Allow ordinal dates as input/output
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ISO8601_Time
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use ESMF
   implicit none

!   private
   public :: DD
   public :: TD
   public :: TP
   public :: TZ

   !public :: string_to_integer_time
   public :: string_to_integer_date
   public :: string_to_esmf_time
   !public :: string_to_esmf_timeinterval
!   public :: operator(==)
!
!   interface operator(==)
!      module procedure :: datefields_equal
!      module procedure :: timefields_equal
!      module procedure :: iso_date_equal
!      module procedure :: iso_time_equal
!      module procedure :: iso_datetime_equal
!   end interface

   ! parameters for processing date, time, and datetime strings
   character, parameter :: DD = '-' ! Date Field Delimiter
   character, parameter :: TD = ':' ! Time Field Delimiter
   character, parameter :: TP = 'T' ! Time Prefix Character
   character, parameter :: TZ = 'Z' ! Time Zone Character
   character(len=*), parameter :: FDD = "(i4, 1x, i2, 1x, i2)" ! Format string for delimited date
   character(len=*), parameter :: FUD = "(i4, i2, i2)" ! Format string for undelimited date
   character(len=*), parameter :: FDT = "(1x, i2, 1x, i2, 1x, i2, 1x, i3, 1x)" ! Format string for delimited time
   character(len=*), parameter :: FUT = "(1x, i2, i2, i2, 1x, i3, 1x)" ! Format string for undelimited time
   character(len=*), parameter :: DATE_FORMAT = "(4i,a,2i,a,2i)"
   character(len=*), parameter :: TIME_FORMAT = "(i2,a,2i,a,f)"
   integer, parameter :: DIM_DF = 3 ! Number of fields in date
   integer, parameter :: DIM_TF = 5 ! Number of fields in time

   ! These are open lower(LB) and upper (UB) bounds of components of the date and time.
   integer, parameter :: LB_YEAR = -1 
   integer, parameter :: UB_YEAR = 10000
   integer, parameter :: LB_MONTH = 0
   integer, parameter :: UB_MONTH = 13
   integer, parameter :: NUM_MONTHS = UB_MONTH - 1
   integer, parameter :: LB_DAY = 0
   integer, parameter :: LB_TIME = -1
   integer, parameter :: LB_HOUR = LB_TIME
   integer, parameter :: UB_HOUR = 24
   integer, parameter :: LB_MINUTE = LB_TIME
   integer, parameter :: UB_MINUTE = 60
   integer, parameter :: LB_SECOND = LB_TIME
   integer, parameter :: UB_SECOND = UB_MINUTE
   integer, parameter :: LB_MILLISECOND = -1
   integer, parameter :: UB_MILLISECOND = 1000

   integer, parameter :: TIMEZONE_DEFAULT = 0

   integer, parameter :: ID_WIDTH = 100
   integer, parameter :: ID_DAY = 1
   integer, parameter :: ID_MONTH = ID_WIDTH * ID_DAY
   integer, parameter :: ID_YEAR = ID_WIDTH * ID_MONTH

   type :: date_fields
      integer :: year
      integer :: month
      integer :: day
      logical :: valid
   end type date_fields

   interface date_fields
      module procedure :: construct_date_fields
   end interface date_fields

   type :: time_fields
      integer :: hour
      integer :: minute
      integer :: second
      integer :: millisecond
      integer :: timezone
      logical :: valid
   end type time_fields

   interface time_fields
      module procedure :: construct_time_fields
   end interface time_fields

   type :: ISO8601Date
      type(date_fields) :: fields
   end type ISO8601Date

   interface ISO8601Date
      module procedure :: construct_ISO8601Date
   end interface ISO8601Date

   type :: ISO8601Time
      type(time_fields) :: fields
      integer :: timezone
   end type ISO8601Time

   interface ISO8601Time
      module procedure :: construct_ISO8601Time
   end interface ISO8601Time

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
      procedure, pass(self) :: timezone
   end type ISO8601DateTime

   interface ISO8601DateTime
      module procedure :: construct_ISO8601DateTime
   end interface ISO8601DateTime

   type :: ISO8601Duration
      type(date_fields) :: date
      type(time_fields) :: time
   end type ISO8601Duration

   type :: ISO8601TimeInterval
      type(ISO8601DateTime) :: start_datetime
      type(ISO8601DateTime) :: end_datetime
      integer :: repetitions
   end type ISO8601TimeInterval

contains


! SIMPLE CONSTRUCTORS

   pure function construct_date_fields(fields) result(df)
      integer, dimension(DIM_DF), intent(in) :: fields
      type(date_fields) :: df
      df % year = fields(1)
      df % month = fields(2)
      df % day = fields(3)
      df % valid = is_valid_date(fields)
   end function construct_date_fields

   pure function construct_time_fields(fields) result(tf)
      integer, dimension(DIM_TF), intent(in) :: fields
      type(time_fields) :: tf
      tf % hour = fields(1)
      tf % minute = fields(2)
      tf % second = fields(3)
      tf % millisecond = fields(4)
      tf % timezone = fields(5)
      tf % valid = is_valid_time(fields)
   end function construct_time_fields

! END SIMPLE CONSTRUCTORS


! LOW-LEVEL UTILITY FUNCTIONS FOR STRING PARSERS

   pure function make_char_array(s) result(a)
      character(len=*), intent(in) :: s
      character, dimension(len(s)) :: a
      integer :: i
      do i=1, len(s)
         a(i) = s(i:i)
      end do
   end function make_char_array

   elemental pure logical function is_digit(c)
      character, intent(in) :: c
      integer, parameter :: lb = iachar('0') - 1
      integer, parameter :: ub = iachar('9') + 1
      is_digit = iachar(c) > lb .and. iachar(c) < ub
   end function is_digit

   pure logical function are_digits(s)
      character(len=*), intent(in) :: s
      are_digits = all(is_digit(make_char_array(s)))
   end function are_digits

   pure logical function is_delimited_date(s)
      character(len=*), intent(in) :: s
      is_delimited_date = len(s)==10 .and. are_digits(s(1:4)) &
         .and. s(5:5)==DD .and. are_digits(s(6:7)) .and. s(8:8)==DD &
         .and. are_digits(s(9:10))
   end function
   
   pure logical function is_undelimited_date(s)
      character(len=*), intent(in) :: s
      is_undelimited_date = len(s)==8 .and. are_digits(s)
   end function is_undelimited_date

   pure logical function is_delimited_time(s)
      character(len=*), intent(in) :: s
      
      select case(len(s))
         case(10)
            is_delimited_time = s(1:1)==TP .and. are_digits(s(2:3)) &
               .and. s(4:4)==TD .and. are_digits(s(5:6)) .and. s(7:7)==TD &
               .and. are_digits(s(8:9)) .and. s(10:10)==TZ
         case(14)
            is_delimited_time = s(1:1)==TP .and. are_digits(s(2:3)) &
               .and. s(4:4)==TD .and. are_digits(s(5:6)) .and. s(7:7)==TD &
               .and. are_digits(s(8:9)) .and. s(10:10)=='.' &
               .and. are_digits(s(11:13)) .and. s(14:14)==TZ
         case default
            is_delimited_time = .FALSE.
      end select

   end function is_delimited_time

   pure logical function is_undelimited_time(s)
      character(len=*), intent(in) :: s
      
      select case(len(s))
         case(8)
            is_undelimited_time = s(1:1)==TP .and. are_digits(s(2:7)) &
               .and. s(8:8)==TZ
         case(12)
            is_undelimited_time = s(1:1)==TP .and. are_digits(s(2:7)) &
               .and. s(8:8)=='.' .and. are_digits(s(9:11)) .and. s(12:12)==TZ
         case default
            is_undelimited_time = .FALSE.
      end select

   end function is_undelimited_time

   pure function split_datetime_string(s) result(parts)
      character(len=*), intent(in) :: s
      character(len=len(s)), dimension(2) :: parts
      parts = split(s, TP)
   end function split_datetime_string

   pure function split(s, delimiter) result(parts)
      character(len=*), intent(in) :: s
      character, intent(in) :: delimiter
      integer :: i = -1
      i = index(s, delimiter)
      if(i > 0) then
         parts(1) = adjustl(s(1:i-1))
         parts(2) = adjustl(s(i:len(s)))
      else
         parts(1) = adjustl(s)
         parts(2) = ''
      endif
   end function split

! END LOW-LEVEL UTILITY FUNCTIONS FOR STRING PARSERS
   

! LOW-LEVEL PARSERS

   subroutine parse_isostring(s, fields, fmts, stat)
      character(len=*), intent(in) :: s
      integer, dimension(:), intent(inout) :: fields
      character(len=*), intent(in) :: fmts
      integer, intent(inout) :: stat
      read (s, fmt=fmts, iostat=stat) fields
   end subroutine parse_isostring
      
   subroutine parse_datestring(s, fields, stat)
      character(len=*), intent(in) :: s
      integer, dimension(DIM_DF), intent(out) :: fields
      integer, intent(inout) :: stat
      integer, dimension(DIM_DF) :: f
      if(is_delimited_date(s)) then
         call parse_isostring(s, fields, FDD, stat)
      else if(is_undelimited_date(s)) then
         call parse_isostring(s, fields, FUD, stat)
      else
         stat = -1
      end if  
   end subroutine parse_datestring

   subroutine parse_timestring(s, fields, stat)
      character(len=*), intent(in) :: s
      integer, dimension(DIM_TF), intent(out) :: fields
      integer, intent(inout) :: stat

      fields(size(fields)) = TIMEZONE_DEFAULT

      if(is_delimited_time(s)) then
         call parse_isostring(s, fields(1:4), FDT, stat)
      else if(is_undelimited_time(s)) then
         call parse_isostring(s, fields(1:4), FUT, stat)
      else
         stat = -1
      end if
   end subroutine parse_timestring

   subroutine parse_datetimestring(s, datefields, timefields, stat)
      character(len=*), intent(in) :: s
      integer, dimension(DIM_DF), intent(out) :: datefields
      integer, dimension(DIM_TF), intent(out) :: timefields
      integer, intent(inout) :: stat
      character(len=len(s)), dimension(2) :: parts 
 
      parts = split_datetime_string(s) 
      call parse_datestring(trim(parts(1)),datefields,stat)
      if(stat == 0) then
         call parse_timestring(trim(parts(2)),timefields,stat)
      else
         stat = -1
      endif
   end subroutine parse_datetimestring
   
! END LOW-LEVEL PARSERS


! LOW-LEVEL DATE & TIME PROCESSING UTILITIES
   ! Return true if factor divides dividend evenly, false otherwise
   pure logical function is_factor(dividend, factor)
      integer, intent(in) :: dividend
      integer, intent(in) :: factor
      ! mod returns the remainder of dividend/factor, and if it is 0, factor divides dividend evenly
      if(factor /= 0) then ! To avoid divide by 0
          is_factor = mod(dividend, factor)==0
      else
          is_factor = .false.
      endif
   end function is_factor

   ! Return true if factor does not divide dividend evenly, false otherwise
   ! see is_factor
   pure logical function is_not_factor(dividend, factor)
      integer, intent(in) :: dividend
      integer, intent(in) :: factor
      is_not_factor = .not. is_factor(dividend, factor)
   end function is_not_factor

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
      is_leap_year = is_factor(y,400) .or. ( is_factor(y, 4) .and. is_not_factor(y, 100) )
   end function is_leap_year

   ! Return the last day numbers of each month based on the year
   pure function get_month_ends(y) result(month_ends)
      integer, intent(in) :: y
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
      integer, dimension(NUM_MONTHS) :: month_ends

      month_ends = get_month_ends(y)    
      get_month_end = month_ends(m)

   end function get_month_end

   pure logical function is_valid_year(y)
      integer, intent(in) :: y
      is_valid_year = is_between(LB_YEAR, UB_YEAR, y)
   end function is_valid_year 

   pure logical function is_valid_month(m)
      integer, intent(in) :: m
      is_valid_month = is_between(LB_MONTH, UB_MONTH, m)
   end function is_valid_month

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

   pure logical function is_valid_hour(hour)
      integer, intent(in) :: hour
      is_valid_hour = is_between(LB_HOUR, UB_HOUR, hour)
   end function is_valid_hour 

   pure logical function is_valid_minute(minute)
      integer, intent(in) :: minute
      is_valid_minute = is_between(LB_MINUTE, UB_MINUTE, minute)
   end function is_valid_minute 

   pure logical function is_valid_second(second)
      integer, intent(in) :: second
      is_valid_second = is_between(LB_SECOND, UB_SECOND, second)
   end function is_valid_second 

   pure logical function is_valid_millisecond(millisecond)
      integer, intent(in) :: millisecond
      is_valid_millisecond = is_between(LB_MILLISECOND, UB_MILLISECOND, millisecond)
   end function is_valid_millisecond 

   pure logical function is_valid_timezone(timezone)
      integer, intent(in) :: timezone
      is_valid_timezone = (timezone == TIMEZONE_DEFAULT)
   end function is_valid_timezone

! END LOW-LEVEL DATE & TIME PROCESSING UTILITIES


! DATE & TIME VERIFICATION

!   pure logical function is_valid_day(df)
!      type(date_fields), intent(in) :: df
!      is_valid_day = is_valid_day_ints(df % year, df % month, df % day)
!   end function is_valid_day
   pure logical function is_valid_date(date)
      integer, dimension(DIM_DF), intent(in) :: date
      integer :: month_end
      integer :: year = date(1)
      integer :: month = date(2)
      integer :: day = date(3)

      if(is_valid_year(year) .and. is_valid_month(month)) then
         month_end = get_month_end(year, month)
         is_valid_date = is_between(LB_DAY, month_end+1, day) 
      else
         is_valid_date = .FALSE.
      endif
   end function is_valid_date

   pure logical function is_valid_time(time)
      integer, dimension(DIM_TF), intent(in) :: time
      integer :: hour = time(1)
      integer :: minute = time(2)
      integer :: second = time(3)
      integer :: millisecond = time(4)
      integer :: timezone = time(5)
      
      is_valid_time = &
         is_valid_hour(hour) .and. &
         is_valid_minute(minute) .and. &
         is_valid_second(second) .and. &
         is_valid_millisecond(millisecond) .and. &
         is_valid_timezone(timezone)
   end function is_valid_time

! END DATE & TIME VERIFICATION


! HIGH-LEVEL CONSTRUCTORS

   pure function construct_ISO8601Date(datefields) result(date)
      type(date_fields), intent(in) :: datefields
      type(ISO8601Date) :: date
      date % fields = datefields
   end function construct_ISO8601Date

   pure function construct_ISO8601Time(timefields, timezone) result(time)
      type(time_fields), intent(in) :: timefields
      integer, intent(in) :: timezone
      type(ISO8601Time) :: time
      time % fields = timefields
      time % timezone = timezone
   end function construct_ISO8601Time

   pure function construct_ISO8601DateTime(date, time) result(datetime)
      type(ISO8601Date), intent(in) :: date
      type(ISO8601Time), intent(in) :: time
      type(ISO8601DateTime) :: datetime
      datetime % date = date
      datetime % time = time
   end function construct_ISO8601DateTime

! END HIGH-LEVEL CONSTRUCTORS


! TYPE-BOUND METHODS

   function year(datetime) result(y)
      class(ISO8601DateTime), intent(in) :: datetime
      integer :: y
      y = datetime % date % year
   end function year

   function month(datetime) result(m)
      class(ISO8601DateTime), intent(in) :: datetime
      integer :: m
      m = datetime % date % month
   end function month

   function day(datetime) result(d)
      class(ISO8601DateTime), intent(in) :: datetime
      integer :: d
      d = datetime % date % day
   end function day

   function hour(datetime) result(h)
      class(ISO8601DateTime), intent(in) :: datetime
      integer :: h
      h = datetime % time % hour
   end function hour

   function minute(datetime) result(m)
      class(ISO8601DateTime), intent(in) :: datetime
      integer :: m
      m = datetime % time % minute
   end function minute

   function second(datetime) result(s)
      class(ISO8601DateTime), intent(in) :: datetime
      integer :: s
      s = datetime % time % second
   end function second

   function millisecond(datetime) result(ms)
      class(ISO8601DateTime), intent(in) :: datetime
      integer :: ms
      ms = datetime % time % millisecond
   end function millisecond

   function timezone(datetime) result(tz)
      class(ISO8601DateTime), intent(in) :: datetime
      integer :: tz
      tz = datetime % time % timezone
   end function timezone

! END TYPE-BOUND METHODS


!! OPERATORS
!   pure function datefields_equal(datefields_a, datefields_b) result(truth_value)
!      type(date_fields), intent(in) :: datefields_a
!      type(date_fields), intent(in) :: datefields_b
!      logical :: truth_value
!      truth_value = (datefields_a % year == datefields_b % year) &
!         .and. (datefields_a % month == datefields_b % month) &
!         .and. (datefields_a % day == datefields_b % day)
!   end function datefields_equal
!   
!   pure function timefields_equal(timefields_a, timefields_b) result(truth_value)
!      type(time_fields), intent(in) :: timefields_a
!      type(time_fields), intent(in) :: timefields_b
!      logical :: truth_value
!      truth_value = (timefields_a % hour == timefields_b % hour) &
!         .and. (timefields_a % minute == timefields_b % minute) &
!         .and. (timefields_a % second == timefields_b % second) &
!         .and. (timefields_a % millisecond == timefields_b % millisecond)
!   end function timefields_equal
!
!   pure function iso_date_equal(date_a, date_b) result(truth_value)
!      type(ISO8601Date), intent(in) :: date_a
!      type(ISO8601Date), intent(in) :: date_b
!      logical :: truth_value
!      truth_value = datefields_equal(date_a % fields, date_b % fields)
!   end function iso_date_equal
!
!   pure function iso_time_equal(time_a, time_b) result(truth_value)
!      type(ISO8601Date), intent(in) :: time_a
!      type(ISO8601Date), intent(in) :: time_b
!      logical :: truth_value
!      truth_value = timefields_equal(time_a % fields, time_b % fields) &
!         .and. time_a % timezone == time_b % timezone
!   end function iso_time_equal
!
!   pure function iso_datetime_equal(datetime_a, datetime_b) result(truth_value)
!      type(ISO8601DateTime), intent(in) :: datetime_a
!      type(ISO8601DateTime), intent(in) :: datetime_b
!      logical :: truth_value
!      truth_value = iso_date_equal(datetime_a % date, datetime_b % date) &
!         .and. iso_time_equal(datetime_a % time, datetime_b % time)
!   end function iso_datetime_equal
!
!! END OPERATORS


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
   function process_date(date_string) result(date)
      character(len=*), intent(in) :: date_string
      type(ISO8601Date) :: date
      type(date_fields) :: df
      integer, dimension(DIM_DF) :: fields
      integer :: stat = -1
      
      call parse_datestring(date_string, fields, stat)
      if(stat == 0) then
         df = date_fields(fields)
         if(df % valid) then
            date = ISO8601Date(df)
         else
            stat == -1
         end if
      end if
   end function process_date

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
   function process_time(time_string) result(time)
      character(len=*), intent(in) :: time_string
      type(ISO8601Date) :: time
      type(time_fields) :: tf
      integer, dimension(DIM_TF) :: fields
      integer :: stat = -1

      call parse_timestring(time_string, fields, stat)
      if(stat == 0) then
         tf = time_fields(fields)
         if(tf % valid) then
            time = ISO8601Time(df)
         else
            stat = -1
         end if
      end if
   end function process_time

   ! parse datetime_string to create a ISO8601DateTime
   ! ISO 8601 defines several datetime formats. This function parses these formats:
   !  YYYYMMDDTHHMMSS.sssZ
   !  YYYY-MM-DDTHH:MM:SS.sssZ
   ! See process_date and process_time for more detail on strings
   function process_datetime(datetime_string) result(datetime)
      character(len=*), intent(in) :: datetime_string
      integer, dimension(DIM_DF) :: datefields
      integer, dimension(DIM_TF) :: timefields
      type(ISO8601DateTime) :: datetime
      integer :: stat 
       
      call parse_datetimestring(datetime_string, datefields, timefields, stat)
      if(stat==0) then
         df = date_fields(datefields)
         tf = time_fields(timefields)
         if((df % valid) .and. (tf % valid)) then
            datetime = ISO8601DateTime(ISO8601Date(df), ISO8601Time(tf))
         else
            stat == -1
         end if
      end if
   end function process_datetime
   
   function string_to_esmf_time(string, unusable, rc) result(time)
      character(len=*), intent(in) :: string
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(ESMF_Time) :: time
 
      type(ISO8601_DateTime) :: datetime
      
      _UNUSED_DUMMY(unusable)

      datetime = process_datetime(string)

      call ESMF_TimeSet(time,yy = datetime % year(), mm = datetime % month(), &
         dd = datetime % day(), h = datetime % hour(), m = datetime % minute(), &
         s= datetime & second(), __RC__)

   end function string_to_esmf_time

   function string_to_integer_date(string, unusable, rc) result(integer_date)
      character(len=*), intent(in) :: string
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: integer_date

      type(ISO8601_Date) :: date
      
      _UNUSED_DUMMY(unusable)

      date = process_date(string)

      integer_date = 
      
   end function string_to_integer_date
end module MAPL_ISO8601_Time
