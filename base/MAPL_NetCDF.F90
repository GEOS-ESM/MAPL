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

   ! LOW-LEVEL - keep commented out (private) unless debugging these procedures
   public :: convert_NetCDF_DateTimeString_to_ESMF_Time, make_NetCDF_DateTime_duration, is_digit_string, get_shift_sign
   public :: make_ESMF_TimeInterval, is_valid_netcdf_datetime_string, convert_to_integer, convert_to_real, split

   interface make_ESMF_TimeInterval
      module procedure :: make_ESMF_TimeInterval_integer
      module procedure :: make_ESMF_TimeInterval_real
   end interface make_ESMF_TimeInterval

   interface make_NetCDF_DateTime_duration
      module procedure :: make_NetCDF_DateTime_duration_integer
      module procedure :: make_NetCDF_DateTime_duration_real
   end interface make_NetCDF_DateTime_duration
   
   interface split
      module procedure :: split_characters
   end interface split

   character, parameter :: PART_DELIM = ' '
   character, parameter :: DATE_DELIM = '-'
   character, parameter :: TIME_DELIM = ':'
   character, parameter :: DELIMS(3) = [PART_DELIM, DATE_DELIM, TIME_DELIM]
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
   integer, parameter :: MAX_CHARACTER_LENGTH = 64

contains

!===============================================================================
!========================= HIGH-LEVEL PROCEDURES ===========================

   ! Get NetCDF DateTime duration from ESMF_Time and units_string (integer)
   subroutine get_NetCDF_duration_from_ESMF_Time_integer(time, units_string, duration, unusable, rc)
      type(ESMF_Time),  intent(inout) :: time
      character(len=*), intent(in) :: units_string
      integer, intent(out) :: duration
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: start_time
      type(ESMF_TimeInterval) :: interval
      character(len=MAX_CHARACTER_LENGTH) :: units
      character(len=MAX_CHARACTER_LENGTH) :: preposition
      character(len=MAX_CHARACTER_LENGTH) :: datetime_string
      character(len=MAX_CHARACTER_LENGTH) :: remainder
      integer :: status
      integer(ESMF_KIND_I8) :: sign_factor

      _UNUSED_DUMMY(unusable)
      
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      call split(trim(units_string), units, remainder, PART_DELIM)
      call split(trim(remainder), preposition, remainder, PART_DELIM)
      datetime_string = trim(remainder)


      call convert_NetCDF_DateTimeString_to_ESMF_Time(trim(datetime_string), start_time, _RC)
      interval = time - start_time
      call ESMF_TimeIntervalValidate(interval, rc = status)
      _ASSERT(status == ESMF_SUCCESS, 'Invalid ESMF_TimeInterval')

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
      character(len=MAX_CHARACTER_LENGTH) :: units
      character(len=MAX_CHARACTER_LENGTH) :: preposition
      character(len=MAX_CHARACTER_LENGTH) :: datetime_string
      character(len=MAX_CHARACTER_LENGTH) :: remainder
      integer :: status
      integer(ESMF_KIND_I8) :: sign_factor

      _UNUSED_DUMMY(unusable)
      
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      call split(trim(units_string), units, remainder, PART_DELIM)
      call split(trim(remainder), preposition, remainder, PART_DELIM)
      datetime_string = trim(remainder)

      call convert_NetCDF_DateTimeString_to_ESMF_Time(datetime_string, start_time, _RC)
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

      type(ESMF_TimeInterval) :: interval
      type(ESMF_Time) :: start_time
      character(len=MAX_CHARACTER_LENGTH) :: units
      character(len=MAX_CHARACTER_LENGTH) :: preposition
      character(len=MAX_CHARACTER_LENGTH) :: datetime_string
      character(len=MAX_CHARACTER_LENGTH) :: remainder
      integer :: signed_duration, sign_factor
      integer :: status

      _UNUSED_DUMMY(unusable)

      _ASSERT(duration >= 0, 'Negative duration not supported')
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      call split(trim(units_string), units, remainder, PART_DELIM)
      call split(trim(remainder), preposition, remainder, PART_DELIM)
      datetime_string = trim(remainder)

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

      type(ESMF_TimeInterval) :: interval
      type(ESMF_Time) :: start_time
      character(len=MAX_CHARACTER_LENGTH) :: units
      character(len=MAX_CHARACTER_LENGTH) :: preposition
      character(len=MAX_CHARACTER_LENGTH) :: datetime_string
      character(len=MAX_CHARACTER_LENGTH) :: remainder
      real(kind=ESMF_KIND_R8) :: signed_duration, sign_factor
      integer :: status

      _UNUSED_DUMMY(unusable)

      _ASSERT(duration >= 0, 'Negative duration not supported')
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      call split(trim(units_string), units, remainder, PART_DELIM)
      call split(trim(remainder), preposition, remainder, PART_DELIM)
      datetime_string = trim(remainder)

      sign_factor = get_shift_sign(preposition)
      _ASSERT(sign_factor /= 0, 'Unrecognized preposition')
      signed_duration = sign_factor * duration

      call convert_NetCDF_DateTimeString_to_ESMF_Time(datetime_string, start_time, _RC)
      call make_ESMF_TimeInterval(signed_duration, units, start_time, interval, _RC)

      time = start_time + interval

      _RETURN(_SUCCESS)

   end subroutine get_ESMF_Time_from_NetCDF_DateTime_real

!======================= END HIGH-LEVEL PROCEDURES =========================
!===============================================================================
!========================= LOWER-LEVEL PROCEDURES ==========================

   ! Convert NetCDF datetime to ESMF_Time
   subroutine convert_NetCDF_DateTimeString_to_ESMF_Time_prior(datetime_string, datetime, unusable, rc)
      character(len=*), intent(in) :: datetime_string
      type(ESMF_Time), intent(inout) :: datetime
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status 
      integer :: yy, mm, dd, h, m, s, i, j 
      character(len=4) :: part
      character(len=:), allocatable :: msg

      _UNUSED_DUMMY(unusable)

      msg = 'Invalid datetime string: ' // datetime_string
      _ASSERT(is_valid_netcdf_datetime_string(datetime_string), msg) 
         

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
      integer :: yy, mm, dd, h, m
      real(kind=ESMF_KIND_R8) :: s_r8
      character(len=MAX_CHARACTER_LENGTH) :: part
      character(len=MAX_CHARACTER_LENGTH) :: remainder

      _UNUSED_DUMMY(unusable)

      _ASSERT(is_valid_netcdf_datetime_string(datetime_string), 'Invalid NetCDF datetime string')

      ! convert first 3 substrings to year, month, day
      remainder = datetime_string

      call split(trim(remainder), part, remainder, DATE_DELIM)
      call convert_to_integer(trim(part), yy, rc = status)
      _ASSERT(status == 0, 'Unable to convert year string')

      call split(trim(remainder), part, remainder, DATE_DELIM)
      call convert_to_integer(trim(part), mm, rc = status)
      _ASSERT(status == 0, 'Unable to convert month string')

      call split(trim(remainder), part, remainder, PART_DELIM)
      call convert_to_integer(trim(part), dd, rc = status)
      _ASSERT(status == 0, 'Unable to convert day string')

      ! convert second 3 substrings to hour, minute, second
      call split(trim(remainder), part, remainder, TIME_DELIM)
      call convert_to_integer(part, h, rc = status)
      _ASSERT(status == 0, 'Unable to convert hour string')

      call split(trim(remainder), part, remainder, TIME_DELIM)
      call convert_to_integer(trim(part), m, rc = status)
      _ASSERT(status == 0, 'Unable to convert minute string')

      part = remainder
      call convert_to_real(trim(part), s_r8, rc = status)
      _ASSERT(status == 0, 'Unable to convert second string')

      ! no need to call this unless datetime units are correct
      call ESMF_CalendarSetDefault(CALKIND_FLAG, _RC)

      call ESMF_TimeSet(datetime, yy=yy, mm=mm, dd=dd, h=h, m=m, s_r8=s_r8, _RC)

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
         case('months')
            _FAIL('Real values for months are not supported.')
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

!======================= END LOWER-LEVEL PROCEDURES ========================
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

   subroutine split_characters(characters, token, remainder, delimiters)
      character(len=*), intent(in) :: characters
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: token
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: remainder
      character(len=*), optional, intent(in) :: delimiters
      character(len=:), allocatable :: delims
      integer :: i

      delims = ' '
      if(present(delimiters)) delims = delimiters
      
      i = scan(characters, delims)

      if(i > 0) then
         token = characters(:(i-1))
         remainder = characters((i+1):) 
      else
         token = characters
         remainder = EMPTY_STRING
      endif

   end subroutine split_characters

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
