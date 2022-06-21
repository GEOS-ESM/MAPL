!Allow 60 for seconds
!No fractions other than milliseconds (explicit)
!No weeks
!Z required for time
!No parts of day
!Allow ordinal dates as input/output

module MAPL_ISO8601DateTime

   type :: date_fields
      integer :: year
      integer :: month
      integer :: day
   end type date_parts

   type :: time_fields
      integer :: hour
      integer :: minute
      integer :: second
      integer :: millisecond
   end type time_fields

   type :: ISO8601Date
      type(date_fields) :: fields
   end type ISO8601Date

   type :: ISO8601Time
      type(time_fields) :: fields
      integer :: timezone
   end type ISO8601Time

   type :: ISO8601DateTime
      type(ISO8601Date) :: date
      type(ISO8601Time) :: time
   end type ISO8601DateTime

   type :: ISO8601Duration
      type(date_fields) :: date
      type(time_fields) :: time
   end type :: ISO8601Duration

   type :: ISO8601TimeInterval
      type(ISO8601Datetime) :: start_datetime
      type(ISO8601Datetime) :: end_datetime
      integer :: repetitions
   end type ISO8601TimeInterval

   interface ISO8601Date
      module procedure :: constructISO8601Date
   end interface ISO8601Date

   private

   char(len=*), parameter :: DATE_DELIMITER = '-'
   char(len=*), parameter :: TIME_DELIMITER = ':'


contains

   ! Return true if factor divides dividend evenly, false otherwise
   pure elemental logical function is_factor(dividend, factor)
      integer, intent(in) :: dividend
      integer, intent(in) :: factor
      ! mod returns the remainder of dividend/factor, and if it is 0, factor divides dividend evenly
      is_factor = mod(dividend, factor) == 0
   end function is_factor

   ! Return true if factor does not divide dividend evenly, false otherwise
   ! see is_factor
   pure elemental logical function is_not_factor(dividend, factor)
      integer, intent(in) :: dividend
      integer, intent(in) :: factor
      is_not_factor = .not. is_factor(dividend, factor)
   end function is_not_factor

   ! Return true if y is a leap year, false otherwise
   pure elemental logical function is_leap_year(y)
      integer, intent(in) :: y
      ! Leap years are years divisible by 400 or (years divisible by 4 and not divisible by 100)
      is_leap_year = is_factor(y,400) .or. ( is_factor(y, 4) .and. is_not_factor mod(y, 100) )
   end function is_leap_year

   ! Return the last day numbers of each month based on the year
   pure elemental function get_month_ends(y) result(res)
      integer, intent(in) :: y
      integer(12), intent(in) :: res
      ! last day numbers of months for leap years
      integer(12), parameter :: MONTH_END_LEAP = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      ! last day numbers of months for regular years
      integer(12), parameter :: MONTH_END = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      if is_leap(y) then
         res = MONTH_END_LEAP 
      else
         res = MONTH_END
      endif
   end function get_month_ends

   ! Return the last day number of month m in year y
   pure elemental integer function get_month_end(y, m)
      integer, intent(in) :: y
      integer, intent(in) :: m
      integer :: month_ends

      month_ends = get_month_ends(y)    
      get_month_end = month_ends(m)

   end function get_month_end
   
   ! Return the delimiter for the date.
   ! This allows changing the delimiters if necessary in the future.
   pure function get_date_delimiter()
      char(len=*), intent(out) :: delimiter
      delimiter = DATE_DELIMITER
   end function get_date_delimiter

   ! Return the delimiter for the date.
   ! This allows changing the delimiters if necessary in the future.
   pure function get_time_delimiter()
      char(len=*), intent(out) :: delimiter
      delimiter = TIME_DELIMITER
   end function get_time_delimiter

   ! Return s with leading and trailing blank space removed
   elemental pure function all_trim(s)
      char(len=*), intent(in) :: s
      char(len=*), intent(out) :: t
      t = trim(adjustl(s))
   end function all_trim 

   ! Construct an ISO8601Date
   pure type(ISO8601Date) function constructISO8601Date(y, m, d) result(date)
      integer, intent(in) :: y
      integer, intent(in) :: m
      integer, intent(in) :: d
      date = ISO8601Date % year = y
      date = ISO8601Date % month = m
      date = ISO8601Date % day = d
   end function constructISO8601Date
      
   pure logical function is_good_year(y)
      integer, intend(in) :: y
      is_good_year = y >= 0 .and. y < 10000
   end function is_good_year 

   pure logical function is_good_month(y, m)
      integer, intent(in) :: y
      integer, intent(in) :: m
      integer(:) :: month_ends
      if(is_good_year(y)) then
         month_ends = get_month
      is_good_month = m > 0 .and. y < 13
   function is_good_month

   pure logical function is_good_day(y, m, d)
      integer, intent(in) :: y
      integer, intent(in) :: m
      integer, intent(in) :: d
      integer, parameter :: month_end

      if(is_good_year(y) .and. is_good_month(m)) then
         month_end = get_month_end(m, y)
         is_good_day = d > 0 .and. d < month_end+1
      else
         is_good_day = .FALSE.
      endif
   end function is_good_day
      
   end function constructISO8601Date
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
   pure elemental ISO8601Date function process_date(date_string) res(date)
      char(len=*), intent(in) :: date_string
      type(ISO8601Date), intent(out) :: date
      char(len=*), parameter :: delimiter = get_date_delimiter()
      integer, parameter :: len_undelimited = len('YYYYMMDD')
      integer, parameter :: len_delimited   = len_undelimited + 2 * len(delimiter)
      char(len=4) :: year
      char(len=2) :: month
      char(len=2) :: day
      char(len=*) :: trimmed
      trimmed = all_trim(date_string)
      select case (len(trimmed))
         case len_undelimited
            
         case len_delimited
            
         case default

      end select
   end function process_date

   pure elemental ISO8601Time function process_time(time_string)
      character(len=*) :: time_string
   end function process_time

   pure elemental ISO8601Datetime function process_datetime(datetime_string)
   end function process_datetime


end module MAPL_ISO8601DateTime
