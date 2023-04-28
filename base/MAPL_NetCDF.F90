!wdb todo
!I also have another request: Do we have a subroutine to convert
!From:
!integer: array(2) = [ 20010101 (YYYYMMDD) 010101 (HHMMSS) ]
!
!To:
!ESMF_TIME: with gregorian calendar
!
!And vice versa.

#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_NetCDF.F90

   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use MAPL_DateTimeParsing.F90
   use ESMF

   implicit none
   public :: convert_NetCDF_DateTime_to_ESMF
   public :: convert_ESMF_to_NetCDF_DateTime

   character, parameter :: PART_DELIM = ' '
   character, parameter :: ISO_DELIM = 'T'
   integer, parameter :: LEN_DATE = len('YYYY-MM-DD')
   integer, parameter :: LEN_TIME = len('hh:mm:ss')
   character(len=*), parameter :: TIME_UNITS =  ['years', 'months', 'days', 'hours', 'minutes', 'seconds', 'milliseconds']
   character, parameter :: SPACE = ' '

contains

! wdb split into two subroutines todo
   subroutine convert_NetCDF_DateTime_to_ESMF(int_time, units_string, interval, time0, unusable, time1, tunit, rc)
      integer, intent(in) :: int_time
      character(len=*), intent(in) :: units_string
      type(ESMF_TimeInterval), intent(out) :: interval
      type(ESMF_Time), intent(out) :: time0
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(out) :: time1
      character(len=:), allocatable, optional, intent(out) :: tunit
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: tunit_
      character(len=:), allocatable :: datetime_string
      character(len=len_trim(units_string)) :: parts(2)
      integer :: span, factor
      integer :: status

      _UNUSED_DUMMY(unusable)

      _ASSERT(int_time >= 0, 'Negative span not supported')
      _ASSERT((len(lr_trim(units_string)) > 0), 'units empty')

      ! get time unit, tunit
      parts = split(lr_trim(units_string), PART_DELIM)
      tunit_ = lr_trim(parts(1))
      _ASSERT(is_time_unit(tunit_), 'Unrecognized time unit')
      if(present(tunit)) tunit = tunit_

      ! get span
      parts = split(lr_trim(parts(2)), PART_DELIM)
      factor = get_shift_sign(parts(1))
      _ASSERT(factor /= 0, 'Unrecognized preposition')
      span = factor * int_time

      call convert_NetCDF_DateTimeString_to_ESMF_Time(lr_trim(parts(2)), t0, _RC)
      call make_ESMF_TimeInterval(span, tunit_, t0, interval, _RC)

      ! get t1
      if(present(t1)) t1 = t0 + interval

      _RETURN(_SUCCESS)

   end subroutine convert_NetCDF_DateTime_to_ESMF

! wdb split into two subroutines todo
   subroutine convert_ESMF_to_NetCDF_DateTime(tunit, t0, int_time, units_string, unusable, t1, interval, rc)
      character(len=*), intent(in) :: tunit
      type(ESMF_Time),  intent(in) :: t0
      integer, intent(out) :: int_time
      character(len=*), intent(out) :: units_string
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(in) :: t1
      type(ESMF_TimeInterval), optional, intent(in) :: interval
      integer, optional, intent(out) :: rc
      type(ESMF_TimeInterval) :: interval_
      integer(ESMF_KIND_I4) :: yy, mm, d, h, m, s
      integer :: status

      _UNUSED_DUMMY(unusable)

      if(present(interval)) then
         interval_ = interval
      elseif(present(t1)) then
         interval_ = t1 - t0
      else
         _ASSERT(has_two_inputs, 'Only one input argument present')
      end if

      call make_NetCDF_DateTime_int_time(interval_, t0, tunit_, int_time, rc)
      call make_NetCDF_DateTime_units_string(interval_, t0, tunit_, units_string, rc)

      _RETURN(_SUCCESS)
      
   end subroutine convert_ESMF_to_NetCDF_DateTime

   subroutine make_ESMF_TimeInterval(span, tunit, t0, interval, unusable, rc)
      integer, intent(in) :: span
      character(len=*), intent(in) :: tunit
      type(ESMF_Time), intent(in) :: t0
      type(ESMF_TimeInterval), intent(out) :: interval
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      select case(lr_trim(tunit)) 
         case('years')
            call ESMF_TimeIntervalSet(interval, startTime=t0 yy=span, _RC)
         case('months')
            call ESMF_TimeIntervalSet(interval, startTime=t0 mm=span, _RC)
         case('days')
            call ESMF_TimeIntervalSet(interval, startTime=t0 d=span, _RC)
         case('hours')
            call ESMF_TimeIntervalSet(interval, startTime=t0 h=span, _RC)
         case('minutes')
            call ESMF_TimeIntervalSet(interval, startTime=t0 m=span, _RC)
         case('seconds')
            call ESMF_TimeIntervalSet(interval, startTime=t0 s=span, _RC)
         case default
            _FAIL('Unrecognized unit')
      end select

      _RETURN(_SUCCESS)

   end subroutine make_ESMF_TimeInterval

   subroutine make_NetCDF_DateTime_int_time(interval, t0, tunit, int_time, unusable, rc)
      type(ESMF_TimeInterval), intent(in) :: interval
      type(ESMF_Time), intent(in) :: t0
      character(len=*), intent(in) :: tunit
      integer, intent(out) :: int_time
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      ! get int_time
      select case(lr_trim(tunit)) 
         case('years')
            call ESMF_TimeIntervalGet(interval, t0, yy=int_time, _RC)
         case('months')
            call ESMF_TimeIntervalGet(interval, t0, mm=int_time, _RC)
         case('days')
            call ESMF_TimeIntervalGet(interval, t0, d=int_time, _RC)
         case('hours')
            call ESMF_TimeIntervalGet(interval, t0, h=int_time, _RC)
         case('minutes')
            call ESMF_TimeIntervalGet(interval, t0, m=int_time, _RC)
         case('seconds')
            call ESMF_TimeIntervalGet(interval, t0, s=int_time, _RC)
         case default
            _FAIL('Unrecognized unit')
      end select

      _RETURN(_SUCCESS)

   end subroutine make_NetCDF_DateTime_int_time

   subroutine make_NetCDF_DateTime_units_string(interval, t0, tunit, units_string, unusable, rc)
      type(ESMF_TimeInterval), intent(in) :: interval
      type(ESMF_Time), intent(in) :: t0
      character(len=*), intent(in) :: tunit
      character(len=:), intent(out) :: units_string
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: preposition = 'since'
      character(len=:), allocatable :: datetime_string
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      ! make units_string
      call convert_ESMF_Time_to_NetCDF_DateTimeString(t0, datetime_string, _RC)
      units_string = tunit //SPACE// preposition //SPACE// datetime_string

      _RETURN(_SUCCESS)

   end subroutine make_NetCDF_DateTime_units_string

   subroutine convert_ESMF_Time_to_NetCDF_DateTimeString(esmf_datetime, datetime_string, unusable, rc)
      type(ESMF_Time), intent(in) :: esmf_datetime
      character(len=:), allocatable, intent(out) :: datetime_string
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: parts(2)
      character(len=:), allocatable :: date_string
      character(len=:), allocatable :: time_string
      integer :: status

      _UNUSED_DUMMY(unusable)

      call ESMF_TimeGet(esmf_datetime, timeString=datetime_string, _RC)
      parts = split(datetime_string, ISO_DELIM)
      date_string = lr_trim(parts(1))
      time_string = lr_trim(parts(2))
      _ASSERT(len(date_string) == LEN_DATE, 'Incorrect date width')
      _ASSERT(len(time_string) >= LEN_TIME, 'Incorrect time width')
      time_string = time_string(1:LEN_TIME)
      datetime_string = date_string //PART_DELIM// time_string
      
      _RETURN(_SUCCESS)

   end subroutine convert_ESMF_Time_to_NetCDF_DateTimeString
   
   subroutine convert_NetCDF_DateTimeString_to_ESMF_Time(datetime_string, datetime, unusable, rc)
      character(len=*), intent(in) :: datetime_string
      type(ESMF_Time), intent(out) :: datetime
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      character(len=len(datetime_string)) :: iso_string
      integer :: status 

      _UNUSED_DUMMY(unusable)

      call convert_to_ISO8601DateTime(datetime_string, iso_string, _RC)
      call ESMF_TimeSet(datetime, trim(iso_string), _RC)

      _RETURN(_SUCCESS)

   end subroutine convert_NetCDF_DateTimeString_to_ESMF_Time

   function is_time_unit(tunit)
      character(len=*), intent(in) :: tunit
      logical :: is_time_unit
      integer :: i

      is_time_unit = .TRUE.
      do i = 1, size(TIME_UNITS)
         if(lr_trim(tunit) == lr_trim(TIME_UNITS(i))) return
      end do
      is_time_unit = .FALSE.

   end function is_time_unit

   function lr_trim(string)
      character(len=*), intent(in) :: string
      character(len=:), allocatable :: lr_trim

      lr_trim = trim(adjustl(string))

   end function lr_trim

   function get_shift_sign(preposition)
      character(len=*), intent(in) :: preposition
      integer :: get_shift_sign
      integer, parameter :: POSITIVE = 1
      get_shift_sign = 0
      if(lr_trim(preposition) == 'since') get_shift_sign = POSITIVE
   end function get_shift_sign

   function split(string, delimiter)
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: delimiter
      character(len=len(string) :: split(2)
      integer start
 
      split = [string, '']
      start = index(string, delimiter)
      if(start < 1) return
      split = [string(1:start - 1), string(start+len(delimiter),len(string))]
   end function split

   function split_all(s, d) result(parts)
      character(len=*), intent(in) :: s
      character(len=*), intent(in) :: d
      character(len=:), allocatable :: parts(:)
      integer :: start

      start = index(s, d)

      if(start == 0) then
         parts = [s]
      else
         parts = [s(1:(start-1)), split_all(s((start+len(d)):len(s)), d)] 
      end if

   end function split_all

   function join(strings, delimiter)
      character(len=*), intent(in) :: strings(:)
      character(len=*), intent(in) :: delimiter
      character(len=:), allocatable :: join
      integer :: i

      join = strings(1)
      do i=2, size(strings)
         join = delimiter  // join
      end do

   end function join
end module MAPL_NetCDF.F90

!   logical function has_delimiter(string, delimiter)
!      character(len=*), intent(in) :: string
!      character(len=*), intent(in) :: delimiter
!
!      has_delimiter = (index(string, delimiter) > 0)
!   end function has_delimiter
!   character, parameter :: DATE_DELIM :: '-'
!   character, parameter :: TIME_DELIM :: ':'

!   subroutine make_ESMF_DateTime(datetime_string, datetime, rc)
!      character(len=*), intent(in) :: datetime_string
!      type(ESMF_Time), intent(out) :: datetime
!      integer, optional, intent(out) :: rc
!      character(len=len(datetime_string)) :: parts(2)
!      type(date_fields) :: date
!      type(time_fields) :: time
!      integer, parameter :: HAS_DATE = 1
!      integer, parameter :: HAS_TIME = 2
!      integer, parameter :: HAS_DATETIME = HAS_DATE + HAS_TIME
!      integer :: i, case_
!      integer :: status
!
!      parts = split(lr_trim(datetime_string), PART_DELIM)
!      _ASSERT((len(parts(1)) == 0), 'First datetime fields missing')
!
!      count_ = 0
!      i = 1
!      if(has_delimiter(parts(i), DATE_DELIM)) then
!         date = date_fields(lr_trim(parts(i)), DATE_DELIM)
!         _ASSERT(.not. date % is_null(), 'Invalid date')
!         case_ = case_ + HAS_DATE
!         i = 2
!      end if
!
!      if(has_delimiter(parts(i), TIME_DELIM)) then
!         time = time_fields(lr_trim(parts(i), TIME_DELIM)
!         _ASSERT(.not. time % is_null(), 'Invalid time')
!         case_ = case_ + HAS_TIME
!      end if
!
!      _ASSERT((count_ > 0) .and. (count_ <= HAS_DATETIME), 'No date or time')
!
!      select case(case_)
!         case(HAS_DATE)
!            call ESMF_TimeSet(datetime, yy = date % year(), mm = date % month(), dd = date % day(), _RC)
!         case(HAS_TIME)
!            call ESMF_TimeSet(datetime, hh = time % hour(), m = time % minute(), s = time % second(), _RC)
!         case(HAS_DATETIME)
!            call ESMF_TimeSet(datetime, yy = date % year(), mm = date % month(), dd = date % day(), &
!               hh = time % hour(), m = time % minute(), s = time % second(), _RC)
!         case default
!            _FAIL('No date or time')
!      end select
!      
!      _RETURN(_SUCCESS)

!   end function make_ESMF_DateTime

!   function parse_timeunit(timeunit_name) result(timeunit)
!      character(len=*), intent(in) :: timeunit_name
!      type(time_unit) :: timeunit
!      character(len=len(timeunit_name)) :: adj_name
!      integer :: trimmed_length
!
!      adj_name = adjustl(timeunit_name)
!      trimmed_length = len_trim(adj_name)
!      if(adj_name(trimmed_length:trimmed_length)
!      timeunit = get_time_unit(trim(timeunit_name))      
!
!   end function parse_timeunit
