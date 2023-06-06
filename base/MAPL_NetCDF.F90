#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
! Procedures to convert between NetCDF datetime and ESMF_Time
! NetCDF datetime is: {integer, character(len=*)}
! {1800, 'seconds since 2010-01-23 18:30:37'}
! {TIME_SPAN, 'TIME_UNIT since YYYY-MM-DD hh:mm:ss'}

module MAPL_NetCDF

   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use MAPL_DateTime_Parsing
   use ESMF

   implicit none

   public :: get_NetCDF_duration_from_ESMF_Time
   public :: get_ESMF_Time_from_NetCDF_DateTime

   interface get_NetCDF_duration_from_ESMF_Time
      module procedure :: get_NetCDF_duration_from_ESMF_Time_integer
      module procedure :: get_NetCDF_duration_from_ESMF_Time_real
   end interface get_NetCDF_duration_from_ESMF_Time

   interface get_ESMF_Time_from_NetCDF_DateTime
      module procedure :: get_ESMF_Time_from_NetCDF_DateTime_integer
      module procedure :: get_ESMF_Time_from_NetCDF_DateTime_real
   end interface get_ESMF_Time_from_NetCDF_DateTime

   private

   ! LOW-LEVEL
!   public :: convert_NetCDF_DateTimeString_to_ESMF_Time, make_NetCDF_DateTime_duration, is_digit_string, get_shift_sign
!   public :: make_ESMF_TimeInterval, is_valid_netcdf_datetime_string, convert_to_integer, convert_to_real, split, split_all

   interface make_ESMF_TimeInterval
      module procedure :: make_ESMF_TimeInterval_integer
      module procedure :: make_ESMF_TimeInterval_real
   end interface make_ESMF_TimeInterval

   interface make_NetCDF_DateTime_duration
      module procedure :: make_NetCDF_DateTime_duration_integer
      module procedure :: make_NetCDF_DateTime_duration_real
   end interface make_NetCDF_DateTime_duration
   
   interface split
      module procedure :: split_chars
   end interface split

   interface split_all
      module procedure :: split_all_iterative
   end interface split_all


   character, parameter :: PART_DELIM = ' '
   character, parameter :: ISO_DELIM  = 'T'
   character, parameter :: DATE_DELIM = '-'
   character, parameter :: TIME_DELIM = ':'
   character, parameter :: POINT = '.'
   character(len=*), parameter :: NETCDF_DATE = '0000' // DATE_DELIM // '00' // DATE_DELIM // '00'
   character(len=*), parameter :: NETCDF_TIME = '00' // TIME_DELIM // '00' // TIME_DELIM // '00'
   character(len=*), parameter :: NETCDF_DATETIME = NETCDF_DATE // PART_DELIM // NETCDF_TIME
   integer, parameter :: LEN_DATE = len(NETCDF_DATE)
   integer, parameter :: LEN_TIME = len(NETCDF_TIME)
   integer, parameter :: LEN_DATETIME = len(NETCDF_DATETIME)
   integer, parameter :: NUM_PARTS_UNITS_STRING = 4
   character(len=*), parameter :: TIME_UNITS(7) = &
      [  'years       ', 'months      ', 'days        ', &
         'hours       ', 'minutes     ', 'seconds     ', 'milliseconds'    ]
   character, parameter :: SPACE = ' '
   type(ESMF_CalKind_Flag), parameter :: CALKIND_FLAG = ESMF_CALKIND_GREGORIAN
   character(len=*), parameter :: DIGIT_CHARS = '0123456789'
   character, parameter :: PLUS = '+'
   character, parameter :: MINUS = '-'
   character(len=*), parameter :: SIGNS = PLUS // MINUS
   character(len=*), parameter :: EMPTY_STRING = ''

contains

!===============================================================================
!========================= NEW HIGH-LEVEL PROCEDURES ===========================

   ! Get NetCDF DateTime duration from ESMF_Time and units_string (integer)
   subroutine get_NetCDF_duration_from_ESMF_Time_integer(time, units_string, duration, unusable, rc)
      type(ESMF_Time),  intent(inout) :: time
      character(len=:), allocatable, intent(in) :: units_string
      integer, intent(out) :: duration
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: start_time
      type(ESMF_TimeInterval) :: interval
      character(len=:), allocatable :: parts(:)
      character(len=:), allocatable :: units
      character(len=:), allocatable :: preposition
      character(len=:), allocatable :: date_string
      character(len=:), allocatable :: time_string
      integer :: status
      integer(ESMF_KIND_I8) :: sign_factor

      _UNUSED_DUMMY(unusable)
      
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      parts = split_all(trim(units_string), PART_DELIM)
      _ASSERT(size(parts) == NUM_PARTS_UNITS_STRING, 'Invalid number of parts in units_string')

      units       = adjustl(parts(1))
      preposition = adjustl(parts(2))
      date_string = adjustl(parts(3))
      time_string = adjustl(parts(4))

      call convert_NetCDF_DateTimeString_to_ESMF_Time(date_string // PART_DELIM // time_string, start_time, _RC)
      interval = time - start_time

      call make_NetCDF_DateTime_duration(interval, start_time, units, duration, _RC)
      sign_factor = get_shift_sign(preposition)
      _ASSERT(sign_factor /= 0, 'Unrecognized preposition')
      duration = sign_factor * duration

      _RETURN(_SUCCESS)
      
   end subroutine get_NetCDF_duration_from_ESMF_Time_integer

   ! Get NetCDF DateTime duration from ESMF_Time and units_string (real)
   subroutine get_NetCDF_duration_from_ESMF_Time_real(time, units_string, duration, unusable, rc)
      type(ESMF_Time),  intent(inout) :: time
      character(len=:), allocatable, intent(in) :: units_string
      real(kind=ESMF_KIND_R8), intent(out) :: duration
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: start_time
      type(ESMF_TimeInterval) :: interval
      character(len=:), allocatable :: parts(:)
      character(len=:), allocatable :: units
      character(len=:), allocatable :: preposition
      character(len=:), allocatable :: date_string
      character(len=:), allocatable :: time_string
      integer :: status
      integer(ESMF_KIND_I8) :: sign_factor

      _UNUSED_DUMMY(unusable)
      
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      parts = split_all(units_string, PART_DELIM)
      _ASSERT(size(parts) == NUM_PARTS_UNITS_STRING, 'Invalid number of parts in units_string')

      units       = adjustl(parts(1))
      preposition = adjustl(parts(2))
      date_string = adjustl(parts(3))
      time_string = adjustl(parts(4))

      call convert_NetCDF_DateTimeString_to_ESMF_Time(date_string // PART_DELIM // time_string, start_time, _RC)
      interval = time - start_time

      call make_NetCDF_DateTime_duration(interval, start_time, units, duration, _RC)
      sign_factor = get_shift_sign(preposition)
      _ASSERT(sign_factor /= 0, 'Unrecognized preposition')
      duration = sign_factor * duration

      _RETURN(_SUCCESS)
      
   end subroutine get_NetCDF_duration_from_ESMF_Time_real

   ! Convert NetCDF datetime {units_string, duration (integer)}
   ! into an ESMF_Time value representing the same datetime
   subroutine get_ESMF_Time_from_NetCDF_DateTime_integer(duration, &
      units_string, time, unusable, rc)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units_string
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), intent(inout) :: time
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: parts(:)
      type(ESMF_TimeInterval) :: interval
      type(ESMF_Time) :: start_time
      character(len=:), allocatable :: units
      character(len=:), allocatable :: preposition
      character(len=LEN_DATE) :: date_string
      character(len=LEN_TIME) :: time_string
      character(len=LEN_DATETIME) :: datetime_string
      integer :: signed_duration, sign_factor
      integer :: status

      _UNUSED_DUMMY(unusable)

      _ASSERT(duration >= 0, 'Negative duration not supported')
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      parts = split_all(units_string, PART_DELIM)
      _ASSERT(size(parts) == NUM_PARTS_UNITS_STRING, 'Invalid number of parts in units_string')

      units       = adjustl(parts(1))
      preposition = adjustl(parts(2))
      date_string = adjustl(parts(3))
      time_string = adjustl(parts(4))
      datetime_string = date_string // PART_DELIM // time_string

      sign_factor = get_shift_sign(preposition)
      _ASSERT(sign_factor /= 0, 'Unrecognized preposition')
      signed_duration = sign_factor * duration

      call convert_NetCDF_DateTimeString_to_ESMF_Time(datetime_string, start_time, _RC)
      call make_ESMF_TimeInterval(signed_duration, units, start_time, interval, _RC)

      time = start_time + interval

      _RETURN(_SUCCESS)

   end subroutine get_ESMF_Time_from_NetCDF_DateTime_integer

   ! Convert NetCDF datetime {units_string, duration (real)}
   ! into an ESMF_Time value representing the same datetime
   subroutine get_ESMF_Time_from_NetCDF_DateTime_real(duration, &
      units_string, time, unusable, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: duration
      character(len=*), intent(in) :: units_string
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), intent(inout) :: time
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: parts(:)
      type(ESMF_TimeInterval) :: interval
      type(ESMF_Time) :: start_time
      character(len=:), allocatable :: units
      character(len=:), allocatable :: preposition
      character(len=:), allocatable :: date_string
      character(len=:), allocatable :: time_string
      character(len=:), allocatable :: datetime_string
      real(kind=ESMF_KIND_R8) :: signed_duration, sign_factor
      integer :: status

      _UNUSED_DUMMY(unusable)

      _ASSERT(duration >= 0, 'Negative duration not supported')
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      parts = split_all(units_string, PART_DELIM)
      _ASSERT(size(parts) == NUM_PARTS_UNITS_STRING, 'Invalid number of parts in units_string')

      units       = adjustl(parts(1))
      preposition = adjustl(parts(2))
      date_string = adjustl(parts(3))
      time_string = adjustl(parts(4))
      datetime_string = date_string // PART_DELIM // time_string

      sign_factor = get_shift_sign(preposition)
      _ASSERT(sign_factor /= 0, 'Unrecognized preposition')
      signed_duration = sign_factor * duration

      call convert_NetCDF_DateTimeString_to_ESMF_Time(datetime_string, start_time, _RC)
      call make_ESMF_TimeInterval(signed_duration, units, start_time, interval, _RC)

      time = start_time + interval

      _RETURN(_SUCCESS)

   end subroutine get_ESMF_Time_from_NetCDF_DateTime_real

!======================= END NEW HIGH-LEVEL PROCEDURES =========================
!===============================================================================
!========================= NEW LOWER-LEVEL PROCEDURES ==========================

   ! Convert NetCDF datetime to ESMF_Time
   subroutine convert_NetCDF_DateTimeString_to_ESMF_Time_prior(datetime_string, datetime, unusable, rc)
      character(len=*), intent(in) :: datetime_string
      type(ESMF_Time), intent(inout) :: datetime
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status 
      integer :: yy, mm, dd, h, m, s, i, j 
      character(len=4) :: part

      _UNUSED_DUMMY(unusable)

      _ASSERT(is_valid_netcdf_datetime_string(datetime_string), &
         'Invalid datetime string: ' // datetime_string)

      i = 1
      j = i + 3
      part = datetime_string(i:j)
      call convert_to_integer(part, yy, rc = status)
      _ASSERT(status == 0, 'Unable to convert year string')

      i = j + 2
      j = j + 3
      part = datetime_string(i:j)
      call convert_to_integer(part, mm, rc = status)
      _ASSERT(status == 0, 'Unable to convert month string')

      i = j + 2
      j = j + 3
      part = datetime_string(i:j)
      call convert_to_integer(part, dd, rc = status)
      _ASSERT(status == 0, 'Unable to convert day string')

      i = j + 2
      j = j + 3
      part = datetime_string(i:j)
      call convert_to_integer(part, h, rc = status)
      _ASSERT(status == 0, 'Unable to convert hour string')

      i = j + 2
      j = j + 3
      part = datetime_string(i:j)
      call convert_to_integer(part, m, rc = status)
      _ASSERT(status == 0, 'Unable to convert minute string')

      i = j + 2
      j = j + 3
      part = datetime_string(i:j)
      call convert_to_integer(part, s, rc = status)
      _ASSERT(status == 0, 'Unable to convert second string')
      call ESMF_CalendarSetDefault(CALKIND_FLAG, _RC)
      call ESMF_TimeSet(datetime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)

      _RETURN(_SUCCESS)

   end subroutine convert_NetCDF_DateTimeString_to_ESMF_Time_prior

   ! Convert NetCDF datetime to ESMF_Time
   subroutine convert_NetCDF_DateTimeString_to_ESMF_Time(datetime_string, datetime, unusable, rc)
      character(len=*), intent(in) :: datetime_string
      type(ESMF_Time), intent(inout) :: datetime
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status 
      integer :: yy, mm, dd, h, m, s
      real(kind=ESMF_KIND_R8) :: s_r8
      character(len=:), allocatable :: part(:)
      character(len=:), allocatable :: date_string
      character(len=:), allocatable :: time_string

      _UNUSED_DUMMY(unusable)

      _ASSERT(is_valid_netcdf_datetime_string(datetime_string), &
         'Invalid datetime string: ' // datetime_string)

      part = split_all(datetime_string, PART_DELIM)
      date_string = part(1)
      time_string = part(2)

      ! convert first 3 substrings to year, month, day
      part = split_all(date_string, DATE_DELIM)

      call convert_to_integer(part(1), yy, rc = status)
      _ASSERT(status == 0, 'Unable to convert year string')

      call convert_to_integer(part(2), mm, rc = status)
      _ASSERT(status == 0, 'Unable to convert month string')

      call convert_to_integer(part(3), dd, rc = status)
      _ASSERT(status == 0, 'Unable to convert day string')

      ! convert second 3 substrings to hour, minute, second
      part = split_all(time_string, TIME_DELIM)

      call convert_to_integer(part(1), h, rc = status)
      _ASSERT(status == 0, 'Unable to convert hour string')

      call convert_to_integer(part(2), m, rc = status)
      _ASSERT(status == 0, 'Unable to convert minute string')

      ! no need to call this unless larger time units are correct
      call ESMF_CalendarSetDefault(CALKIND_FLAG, _RC)

      ! Need to see if the seconds portion has fractional portion and handle it.
      ! Split seconds string to see if it has a fractional part.
      select case(size(split_all(part(3), POINT)))

         ! no fractional portion => use integer
         case(1)
            call convert_to_integer(part(3), s, rc = status)
            _ASSERT(status == 0, 'Unable to convert second string')
            call ESMF_TimeSet(datetime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)

         ! fractional portion => use real(kind=ESMF_KIND_R8)
         case(2)
            call convert_to_real(part(3), s_r8, rc = status)
            _ASSERT(status == 0, 'Unable to convert second string')
            call ESMF_TimeSet(datetime, yy=yy, mm=mm, dd=dd, h=h, m=m, s_r8=s_r8, _RC)

         ! wrong number of substrings => FAIL
         case default
            _FAIL('Incorrect number of second parts')

      end select

      _RETURN(_SUCCESS)

   end subroutine convert_NetCDF_DateTimeString_to_ESMF_Time

   ! Make ESMF_TimeInterval from a span of time, time unit, and start time
   subroutine make_ESMF_TimeInterval_integer(span, tunit, start_time, interval, unusable, rc)
      integer, intent(in) :: span
      character(len=*), intent(in) :: tunit
      type(ESMF_Time), intent(inout) :: start_time
      type(ESMF_TimeInterval), intent(inout) :: interval
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      select case(trim(adjustl(tunit))) 
         case('years')
            call ESMF_TimeIntervalSet(interval, startTime=start_time, yy=span, _RC)
         case('months')
            call ESMF_TimeIntervalSet(interval, startTime=start_time, mm=span, _RC)
         case('hours')
            call ESMF_TimeIntervalSet(interval, startTime=start_time, h=span, _RC)
         case('minutes')
            call ESMF_TimeIntervalSet(interval, startTime=start_time, m=span, _RC)
         case('seconds')
            call ESMF_TimeIntervalSet(interval, startTime=start_time, s=span, _RC)
         case default
            _FAIL('Unrecognized unit')
      end select

      _RETURN(_SUCCESS)

   end subroutine make_ESMF_TimeInterval_integer

   subroutine make_ESMF_TimeInterval_real(span, tunit, start_time, interval, unusable, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: span
      character(len=*), intent(in) :: tunit
      type(ESMF_Time), intent(inout) :: start_time
      type(ESMF_TimeInterval), intent(inout) :: interval
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      select case(trim(adjustl(tunit))) 
         case('years')
            _FAIL('Real values for years are not supported.')
         case('months')
            _FAIL('Real values for months are not supported.')
         case('days')
            _FAIL('Real values for days are not supported.')
         case('hours')
            call ESMF_TimeIntervalSet(interval, startTime=start_time, h_r8=span, _RC)
         case('minutes')
            call ESMF_TimeIntervalSet(interval, startTime=start_time, m_r8=span, _RC)
         case('seconds')
            call ESMF_TimeIntervalSet(interval, startTime=start_time, s_r8=span, _RC)
         case default
            _FAIL('Unrecognized unit')
      end select

      _RETURN(_SUCCESS)

   end subroutine make_ESMF_TimeInterval_real

   ! Get time span from NetCDF datetime
   ! Make NetCDF_DateTime duration from interval, start_time (ESMF_Time), and time units. (integer)
   subroutine make_NetCDF_DateTime_duration_integer(interval, start_time, units, duration, unusable, rc)
      type(ESMF_TimeInterval), intent(inout) :: interval
      type(ESMF_Time), intent(inout) :: start_time
      character(len=*), intent(in) :: units
      integer, intent(out) :: duration
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      ! get duration
      select case(trim(adjustl(units))) 
         case('years')
            call ESMF_TimeIntervalGet(interval, start_time, yy=duration, _RC)
         case('months')
            call ESMF_TimeIntervalGet(interval, start_time, mm=duration, _RC)
         case('hours')
            call ESMF_TimeIntervalGet(interval, start_time, h=duration, _RC)
         case('minutes')
            call ESMF_TimeIntervalGet(interval, start_time, m=duration, _RC)
         case('seconds')
            call ESMF_TimeIntervalGet(interval, start_time, s=duration, _RC)
         case default
            _FAIL('Unrecognized unit')
      end select

      _RETURN(_SUCCESS)

   end subroutine make_NetCDF_DateTime_duration_integer

   ! Get time span from NetCDF datetime
   ! Make NetCDF_DateTime duration from interval, start_time (ESMF_Time), and time units. (real)
   subroutine make_NetCDF_DateTime_duration_real(interval, start_time, units, duration, unusable, rc)
      type(ESMF_TimeInterval), intent(inout) :: interval
      type(ESMF_Time), intent(inout) :: start_time
      character(len=*), intent(in) :: units
      real(kind=ESMF_KIND_R8), intent(out) :: duration
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      ! get duration
      select case(trim(adjustl(units))) 
         case('years')
            _FAIL('Real values for years are not supported.')
!            call ESMF_TimeIntervalGet(interval, start_time, yy=duration, _RC)
         case('months')
            _FAIL('Real values for months are not supported.')
!            call ESMF_TimeIntervalGet(interval, start_time, mm=duration, _RC)
         case('days')
            _FAIL('Real values for days are not supported.')
         case('hours')
            call ESMF_TimeIntervalGet(interval, start_time, h_r8=duration, _RC)
         case('minutes')
            call ESMF_TimeIntervalGet(interval, start_time, m_r8=duration, _RC)
         case('seconds')
            call ESMF_TimeIntervalGet(interval, start_time, s_r8=duration, _RC)
         case default
            _FAIL('Unrecognized unit')
      end select

      _RETURN(_SUCCESS)

   end subroutine make_NetCDF_DateTime_duration_real

!======================= END NEW LOWER-LEVEL PROCEDURES ========================
!===============================================================================
!============================= UTILITY PROCEDURES ==============================

   recursive function is_valid_netcdf_datetime_string(string) result(lval)
      character(len=*), intent(in) :: string
      logical :: lval
      integer :: i

      lval = .false.
      
      i = index(string, POINT)

      if(i == 1) return

      if(i > 0) then
         lval = is_valid_netcdf_datetime_string_real_seconds(string, i)
         return
      end if
      
      if(len(trim(string)) /= len(NETCDF_DATETIME)) return

      do i=1, len_trim(string)
         if(scan(NETCDF_DATETIME(i:i), DIGIT_CHARS) > 0) then
            if(scan(string(i:i), DIGIT_CHARS) <= 0) return
         else
            if(string(i:i) /= NETCDF_DATETIME(i:i)) return
         end if
      end do
      
      lval = .true.

   end function is_valid_netcdf_datetime_string

   function is_valid_netcdf_datetime_string_real_seconds(string, i) result(lval)
      character(len=*), intent(in) :: string
      integer, intent(in) :: i
      logical :: lval

      lval = is_valid_netcdf_datetime_string(string(1:(i-1))) .and. &
         ((i == len(string)) .or. is_digit_string(string((i+1):)))
      
   end function is_valid_netcdf_datetime_string_real_seconds

   ! Get the sign of integer represening a time span based on preposition
   function get_shift_sign(preposition)
      character(len=*), intent(in) :: preposition
      integer :: get_shift_sign
      integer, parameter :: POSITIVE = 1
      get_shift_sign = 0
      if(adjustl(preposition) == 'since') get_shift_sign = POSITIVE
   end function get_shift_sign

   ! Split string at delimiter
   function split_chars(chars, delimiter) result(pair)
      character(len=*), intent(in) :: chars
      character(len=*), intent(in) :: delimiter
      character(len=len(chars)) :: pair(2)
      integer start
 
      pair = ['', '']

      start = index(chars, delimiter)
      if(start == 0) then
         pair(1) = chars
         return
      end if

      pair(1) = chars(1:(start - 1))
      pair(2) = chars((start+len(delimiter)):len_trim(chars))

   end function split_chars

   function split_all_iterative(string, delimiter) result(parts)
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: delimiter
      character(len=:), allocatable :: parts(:)
      character(len=:), allocatable :: pair(:)
      character(len=:), allocatable :: head
      character(len=:), allocatable :: tail
         
      parts = [trim(string)]

      if((len(string) == 0) .or. (len(delimiter) == 0)) return

      tail = parts(1)
      parts = [character::]
      do while (len(tail) > 0)
         pair = split(tail, delimiter)
         head = trim(pair(1))
         tail = trim(pair(2))
         if(len(head) > 0) parts = [parts, head]
      end do

   end function split_all_iterative

   ! Convert string representing an integer to the integer
   subroutine convert_to_integer(string, n, rc)
      character(len=*), intent(in) :: string
      integer, intent(out) :: n
      integer, optional, intent(out) :: rc
      integer :: stat

      n = -1
      read(string, '(I16)', iostat=stat) n

      if(present(rc)) rc = stat

   end subroutine convert_to_integer

   ! Convert string representing a real to a real(REAL64)
   subroutine convert_to_real(string, t, rc)
      character(len=*), intent(in) :: string
      real(kind=ESMF_KIND_R8), intent(out) :: t
      integer, optional, intent(out) :: rc
      integer :: stat

      t = -1
      read(string, *, iostat=stat) t

      if(present(rc)) rc = stat

   end subroutine convert_to_real

   ! Check if string consists of only digit characters
   function is_digit_string(string)
      character(len=*), intent(in) :: string
      logical :: is_digit_string 

      is_digit_string = .FALSE.
      if(len_trim(string) == 0) return

      is_digit_string = (verify(string(:len_trim(string)), DIGIT_CHARS) == 0)

   end function is_digit_string

!=========================== END UTILITY PROCEDURES ============================
!===============================================================================

end module MAPL_NetCDF
