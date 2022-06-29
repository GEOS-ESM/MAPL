!Allow 60 for seconds
!No fractions other than milliseconds (explicit)
!No weeks
!Z required for time
!No parts of day
!Allow ordinal dates as input/output

module MAPL_ISO8601_Time

   private

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

   ! These are open lower(LB) and upper (UB) bounds of components of the date and time.
   integer, parameter :: LB_YEAR = -1 
   integer, parameter :: UB_YEAR = 10000
   integer, parameter :: LB_MONTH = 0
   integer, parameter :: UB_MONTH = 13
   integer, parameter :: NUM_MONTHS = UB_MONTH - 1
   integer, parameter :: LB_DAY = 0

   type :: date_fields
      integer :: year
      integer :: month
      integer :: day
   end type date_fields

   interface date_fields
      module procedure :: construct_date_fields
   end interface date_fields

   type :: time_fields
      integer :: hour
      integer :: minute
      integer :: second
      integer :: millisecond
   end type time_fields

   interface time_fields
      module procedure :: construct_time_fields
   end interface time_fields

   type :: ISO8601Date
      type(date_fields) :: fields
   end type ISO8601Date

   interface ISO8601Date
      module procedure :: construct_ISO8601Date_fields
   end interface ISO8601Date

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
   end type ISO8601Duration

   type :: ISO8601TimeInterval
      type(ISO8601DateTime) :: start_datetime
      type(ISO8601DateTime) :: end_datetime
      integer :: repetitions
   end type ISO8601TimeInterval

contains


! SIMPLE CONSTRUCTORS

   pure function construct_date_fields(f) result(df)
      integer, dimension(3), intent(in) :: f
      type(date_fields) :: df
      df = date_fields(f(1), f(2), f(3))
   end function construct_date_fields

   pure function construct_time_fields(f) result(tf)
      integer, dimension(4), intent(in) :: f
      type(time_fields) :: tf
      tf = time_fields(f(1), f(2), f(3), f(4))
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
         case(10)  ! is(s, d, 7)
            is_delimited_time = s(1:1)==TP .and. are_digits(s(2:3)) &
               .and. s(4:4)==TD .and. are_digits(s(5:6)) .and. s(7:7)==TD &
               .and. are_digits(s(8:9)) .and. s(10:10)==TZ
         case(14)
            is_delimited_time = s(1:1)==TP .and. are_digits(s(2:3)) &
               .and. s(4:4)==TD .and. are_digits(s(5:6)) .and. s(7:7)==TD &
               .and. are_digits(s(8:9)) .and. s(10:10)=='.' &
               .and. are_digits(s(11:13)) .and. s(14:14)==TZ
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
      end select

   end function is_undelimited_time

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
      type(date_fields), intent(out) ::fields
      integer, intent(inout) :: stat
      if(is_delimited_date(s)) then
         call parse_isostring(s, fields, FDD, stat)
      else if(is_undelimited_date(s)) then
         call parse_isostring(s, fields, FUD, stat)
      else
         stat = -1
      end if  
      if(stat==0) then
         df = construct
      endif
   end subroutine parse_datestring

   subroutine parse_timestring(s, fields, stat)
      character(len=*), intent(in) :: s
      integer, dimension(:), intent(in) :: fields
      integer, intent(inout) :: stat
      if(is_delimited_time(s)) then
         call parse_isostring(s, fields, FDT, stat)
      else if(is_undelimited_time(s)) then
         call parse_isostring(s, fields, FUT, stat)
      else
         stat = -1
      end if  
   end subroutine parse_timestring

! END LOW-LEVEL PARSERS


! LOW-LEVEL DATE PROCESSING UTILITIES
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

   pure logical function is_good_year(y)
      integer, intent(in) :: y
      is_good_year = is_between(LB_YEAR, UB_YEAR, y)
   end function is_good_year 

   pure logical function is_good_month(m)
      integer, intent(in) :: m
      is_good_month = is_between(LB_MONTH, UB_MONTH, m)
   end function is_good_month

   pure logical function is_good_day_ints(y, m, d)
      integer, intent(in) :: y
      integer, intent(in) :: m
      integer, intent(in) :: d
      integer :: month_end

      if(is_good_year(y) .and. is_good_month(m)) then
         month_end = get_month_end(y, m)
         is_good_day = is_between(LB_DAY, month_end, d) 
      else
         is_good_day = .FALSE.
      endif
   end function is_good_day_ints
      
! END LOW-LEVEL DATE PROCESSING UTILITIES


! DAY VERIFICATION

   pure logical function is_good_day(df)
      type(date_fields), intent(in) :: df
      is_good_day_fields = is_good_day_ints(df % year, df % month, df % day)
   end logical function is_good_day

! END DAY VERIFICATION


! HIGH-LEVEL CONSTRUCTORS

   pure type(ISO8601Date) function construct_ISO8601Date_fields(df) result(date)
      type(date_fields), intent(in) :: df
      type(ISO8601Date) :: date
      date % fields = df
   end function construct_ISO8601Date_fields

! END HIGH-LEVEL CONSTRUCTORS


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
      integer :: stat 

      call parse_datestring(date_string, df, stat)
      if(stat==0) then
         if(is_good_day(df)) then
            date = ISO8601Date(df)
         else
            stat = -1
         end if
      end if
   end function process_date

   function process_time(time_string) result(time)
      character(len=*), intent(in) :: time_string
      type(ISO8601Time) :: time
   end function process_time

   function process_datetime(datetime_string) result(datetime)
      character(len=*), intent(in) :: datetime_string
      type(ISO8601DateTime) :: datetime
   end function process_datetime

! END HIGH-LEVEL PROCESSORS


end module MAPL_ISO8601_Time
   
!   subroutine get_datefields(isostring, dlm, datefields, stat)
!      character(len=*), intent(in) :: isostring
!      character(len=*), intent(in) :: dlm
!      type(date_fields), intent(inout) :: datefields
!      integer, intent(inout) :: stat
!      integer, dimension(3) :: parts
!      integer :: part_index
!      integer :: p1
!      integer :: p2
!      
!      p1 = 1
!
!      do part_index=1,3
!         p2 = index(isostring(p1:len(isostring)), dlm) 
!         read(isostring(p1:p2-1),*,iostat=stat) parts(parts_index)
!         if(stat /= 0) exit
!         p1 = p2 + len(dlm)
!      end do
!
!      if(stat==0) datefields = date_fields(parts(1), parts(2), parts(3))
!
!   end subroutine get_datefields 
! 
!   subroutine get_datefields(isostring, datefields, stat)
!      character(len=*), intent(in) :: isostring
!      type(date_fields), intent(inout) :: datefields
!      integer, intent(inout) :: stat
!      integer, dimension(3) = parts
!      integer :: part_index
!      integer, dimension(3,2) :: indices = [[1, 4], [5, 6], [7, 8]]
!      
!      do part_index = 1, 3
!         read(isostring(indices(part_index, 1), indices(part_index, 2)),*,iostat=stat) parts(part_index)
!         if(stat /= 0) exit
!      end do
!
!      if(stat==0) datefields = date_fields(parts(1), parts(2), parts(3))
!
!   end subroutine get_datefields
!
!   subroutine get_timefields(isostring, delimiter, timefields, stat)
!      character(len=*), intent(in) :: isostring
!      character, intent(in) :: delimiter
!      type(time_fields), intent(inout) :: timefields
!      integer, intent(inout) :: stat
!      integer, dimension(2) :: dpos = [3, 6]
!      integer, dimension(3) :: indices = [1, 3, 5]
!      integer, dimension(3) :: offsets = [0, 1, 2]
!      real :: seconds
!    
!      timefields = time_fields()
!      
!      if(index(isostring, delimiter) > 0) indices = indices + offsets
!
!      read(isostring(indices(1):indices(1)+1),*,iostat=stat) timefields % hour
!      if(stat /= 0) return
!
!      read(isostring(indices(2):indices(2)+1),*,iostat=stat) timefields % minute
!      if(stat /= 0) return
!
!      read(isostring(indices(3):len_trim(isostring),*,iostat=stat) seconds
!      if(stat /= 0) return
!
!      timefields % second = int(seconds)
!      timefields % millisecond = fraction(seconds)
!
!   end subroutine get_timefields
!   pure is_match(s, d) result (match)
!      character(len=*), intent(in) :: s
!      character, intent(in) :: d
!      logical, dimension(len(s)) :: match
!      
!      do i=1,len(s)
!         match(i) = s(i)==d
!      end do
!   end is_match
!
!   pure logical is_delimited(s, d)
!      character(len=*), intent(in) :: s
!      character, intent(in) :: d
!      return(any(is_match(s, d)))
!   end is_delimited
!
!   function split(s, d, parts)
!      character(len=*), intent(in) :: s
!      character(len=*), intent(in) :: d
!      character(len=len_trim(adjustl(str))), dimension(:), allocatable, intent(out) :: parts
!      integer :: lend = len(d)
!      integer :: lens = len(s)
!      integer :: p = -1
!      integer :: b = 1
!      integer :: e = -1
!
!      
!      p=index(s(b:lens), d)
!      print*, p
!      do while p > 0  
!         e = b + p - 1
!         parts % push_back(s(b:e))
!         b = b + p + lend
!         p=index(s(b:lens), d)      
!      end do
!      
!      e = lens
!      parts % push_back(s(b:e))
!
!   end function split
