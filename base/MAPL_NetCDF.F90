!wdb todo
! todo Switch integer to integer(kind=ESMF_KIND_I8) where appropriate.
!Do REAL(8) days need to be included?
!Do INTEGER or INTEGER(8) days need to be included?
!Is d_r8 Julian day or Gregorian day?
!Does get_shift_sign need to be converted to real for real procedures? 

!subroutine to convert
!From: integer: array(2) = [ 20010101  010101 (HHMMSS) ] ![ (YYYYMMDD) (HHMMSS) ]
!To: !ESMF_TIME: with gregorian calendar
!And vice versa.
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
! Procedures to convert from NetCDF datetime to ESMF_Time and ESMF_TimeInterval
! NetCDF datetime is: {integer, character(len=*)}
! {1800, 'seconds since 2010-01-23 18:30:37'}
! {TIME_SPAN, 'TIME_UNIT since YYYY-MM-DD hh:mm:ss'}
module MAPL_NetCDF

   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use MAPL_DateTime_Parsing
   use ESMF

   implicit none

   public :: convert_NetCDF_DateTime_to_ESMF
   public :: convert_ESMF_to_NetCDF_DateTime
   public :: convert_NetCDF_DateTime_to_ESMF_Time
   public :: make_ESMF_TimeInterval
   public :: make_NetCDF_DateTime_duration
   public :: make_NetCDF_DateTime_units_string
   public :: convert_ESMF_Time_to_NetCDF_DateTimeString 
   public :: convert_to_integer
   public :: convert_NetCDF_DateTimeString_to_ESMF_Time
   public :: is_time_unit
   public :: is_valid_netcdf_datetime_string
   public :: get_shift_sign
   public :: split
   public :: split_all
   public :: get_NetCDF_duration_from_ESMF_Time

   interface convert_NetCDF_DateTime_to_ESMF_Time
      module procedure :: convert_NetCDF_DateTime_to_ESMF_Time_integer
      module procedure :: convert_NetCDF_DateTime_to_ESMF_Time_real
   end interface convert_NetCDF_DateTime_to_ESMF_Time

   interface make_ESMF_TimeInterval
      module procedure :: make_ESMF_TimeInterval_integer
      module procedure :: make_ESMF_TimeInterval_real
   end interface make_ESMF_TimeInterval

   interface make_NetCDF_DateTime_duration
      module procedure :: make_NetCDF_DateTime_duration_integer
      module procedure :: make_NetCDF_DateTime_duration_real
   end interface make_NetCDF_DateTime_duration
   
   interface get_NetCDF_duration_from_ESMF_Time
      module procedure :: get_NetCDF_duration_from_ESMF_Time_integer
      module procedure :: get_NetCDF_duration_from_ESMF_Time_real
   end interface get_NetCDF_duration_from_ESMF_Time

   interface split
      module procedure :: split_chars
   end interface split

   interface split_all
      module procedure :: split_all_recursive
      module procedure :: split_all_iterative
   end interface split_all
   private

   character, parameter :: PART_DELIM = ' '
   character, parameter :: ISO_DELIM  = 'T'
   character, parameter :: DATE_DELIM = '-'
   character, parameter :: TIME_DELIM = ':'
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

contains

!===============================================================================
!========================= OLD HIGH-LEVEL PROCEDURES =========================== 

   ! Convert NetCDF_DateTime {int_time, units_string} to
   ! ESMF time variables {interval, start_time, time} and time unit {tunit}
   ! start_time is the start time, and time is start_time + interval
   subroutine convert_NetCDF_DateTime_to_ESMF(int_time, units_string, &
         interval, start_time, unusable, time, tunit, rc)
      integer, intent(in) :: int_time
      character(len=*), intent(in) :: units_string
      type(ESMF_TimeInterval), intent(inout) :: interval
      type(ESMF_Time), intent(inout) :: start_time
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(inout) :: time
      character(len=:), allocatable, optional, intent(out) :: tunit
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: tunit_
      character(len=len_trim(units_string)) :: parts(2)
      character(len=len_trim(units_string)) :: head
      character(len=len_trim(units_string)) :: tail
      
      integer :: span, factor
      integer :: status

      _UNUSED_DUMMY(unusable)

      _ASSERT(int_time >= 0, 'Negative span not supported')
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units empty')

      ! get time unit, tunit
      parts = split(trim(adjustl(units_string)), PART_DELIM)
      head = parts(1)
      tail = parts(2)
      tunit_ = trim(adjustl(head))
      _ASSERT(is_time_unit(tunit_), 'Unrecognized time unit')
      if(present(tunit)) tunit = tunit_

      ! get span
      parts = split(trim(adjustl(tail)), PART_DELIM)
      head = parts(1)
      tail = parts(2)
      
      factor = get_shift_sign(head)
      _ASSERT(factor /= 0, 'Unrecognized preposition')
      span = factor * int_time

      call convert_NetCDF_DateTimeString_to_ESMF_Time(trim(adjustl(tail)), start_time, _RC)
      call make_ESMF_TimeInterval(span, tunit_, start_time, interval, _RC)

      ! get time
      if(present(time)) time = start_time + interval

      _RETURN(_SUCCESS)

   end subroutine convert_NetCDF_DateTime_to_ESMF

   ! Convert ESMF time variables to an NetCDF datetime
   subroutine convert_ESMF_to_NetCDF_DateTime(tunit, start_time, int_time, units_string, unusable, time, interval, rc)
      character(len=*), intent(in) :: tunit
      type(ESMF_Time),  intent(inout) :: start_time
      integer, intent(out) :: int_time
      character(len=:), allocatable, intent(out) :: units_string
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(inout) :: time
      type(ESMF_TimeInterval), optional, intent(inout) :: interval
      integer, optional, intent(out) :: rc
      type(ESMF_TimeInterval) :: interval_
      integer :: status

      _UNUSED_DUMMY(unusable)

      if(present(interval)) then
         interval_ = interval
      elseif(present(time)) then
         interval_ = time - start_time
      else
         _FAIL( 'Only one input argument present')
      end if

      call make_NetCDF_DateTime_duration(interval_, start_time, tunit, int_time, _RC)
      call make_NetCDF_DateTime_units_string(start_time, tunit, units_string, _RC)

      _RETURN(_SUCCESS)
      
   end subroutine convert_ESMF_to_NetCDF_DateTime

!========================= END OLD HIGH-LEVEL PROCEDURES =======================
!===============================================================================
!========================= OLD LOWER-LEVEL PROCEDURES ==========================

   ! Make 'units' for NetCDF datetime
   subroutine make_NetCDF_DateTime_units_string(start_time, tunit, units_string, unusable, rc)
      type(ESMF_Time), intent(inout) :: start_time
      character(len=*), intent(in) :: tunit
      character(len=:), allocatable, intent(out) :: units_string
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: preposition = 'since'
      character(len=:), allocatable :: datetime_string
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      ! make units_string
      call convert_ESMF_Time_to_NetCDF_DateTimeString(start_time, datetime_string, _RC)
      units_string = tunit //SPACE// preposition //SPACE// datetime_string

      _RETURN(_SUCCESS)

   end subroutine make_NetCDF_DateTime_units_string

   ! Convert ESMF_Time to a NetCDF datetime string (start datetime)
   subroutine convert_ESMF_Time_to_NetCDF_DateTimeString(esmf_datetime, datetime_string, unusable, rc)
      type(ESMF_Time), intent(inout) :: esmf_datetime
      character(len=:), allocatable, intent(out) :: datetime_string
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
 
      character(len=*), parameter :: ERR_PRE = 'Failed to write string: '
      integer :: yy, mm, dd, h, m, s
      character(len=10) :: FMT
      character(len=4) :: yy_string
      character(len=2) :: mm_string
      character(len=2) :: dd_string
      character(len=2) :: h_string
      character(len=2) :: m_string
      character(len=2) :: s_string
      character(len=LEN_DATETIME) :: tmp_string
      integer :: status, iostatus

      _UNUSED_DUMMY(unusable)

      call ESMF_TimeGet(esmf_datetime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)

      FMT='(BZ, I2.2)'
      write(s_string, fmt=FMT, iostat=iostatus) s
      _ASSERT(iostatus == 0, ERR_PRE // 'second')
      write(m_string, fmt=FMT, iostat=iostatus) m
      _ASSERT(iostatus == 0, ERR_PRE // 'minute')
      write(h_string, fmt=FMT, iostat=iostatus) h
      _ASSERT(iostatus == 0, ERR_PRE // 'hour')
      write(dd_string, fmt=FMT, iostat=iostatus) dd
      _ASSERT(iostatus == 0, ERR_PRE // 'day')
      write(mm_string, fmt=FMT, iostat=iostatus) mm
      _ASSERT(iostatus == 0, ERR_PRE // 'month')
      FMT='(BZ, I4.4)'
      write(yy_string, fmt=FMT, iostat=iostatus) yy
      _ASSERT(iostatus == 0, ERR_PRE // 'year')

      tmp_string = yy_string // DATE_DELIM // mm_string // DATE_DELIM // dd_string // PART_DELIM // &
         h_string // TIME_DELIM // m_string // TIME_DELIM // s_string

      datetime_string = tmp_string
      
      _RETURN(_SUCCESS)

   end subroutine convert_ESMF_Time_to_NetCDF_DateTimeString

!======================= END OLD LOWER-LEVEL PROCEDURES ========================
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

   subroutine convert_NetCDF_DateTime_to_ESMF_Time_integer(duration, &
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

   end subroutine convert_NetCDF_DateTime_to_ESMF_Time_integer

   subroutine convert_NetCDF_DateTime_to_ESMF_Time_real(duration, &
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
      character(len=:), allocatable :: datetime_string !wdb fixme deleteme 
      real(kind=ESMF_KIND_R8) :: signed_duration, sign_factor
      integer :: status

      _UNUSED_DUMMY(unusable)

      _ASSERT(duration >= 0, 'Negative duration not supported')
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units_string empty')

      parts = split_all(units_string, PART_DELIM)
      _ASSERT(size(parts) == NUM_PARTS_UNITS_STRING, 'Invalid number of parts in units_string')

      units       = adjustl(parts(1))
      print *, 'units: ', units !wdb fixme deleteme 
      preposition = adjustl(parts(2))
      print *, 'preposition: ', preposition !wdb fixme deleteme 
      date_string = adjustl(parts(3))
      print *, 'date_string: ', date_string !wdb fixme deleteme 
      time_string = adjustl(parts(4))
      print *, 'time_string: ', time_string !wdb fixme deleteme 

      sign_factor = get_shift_sign(preposition)
      _ASSERT(sign_factor /= 0, 'Unrecognized preposition')
      print *, 'sign_factor = ', sign_factor !wdb fixme deleteme 
      signed_duration = sign_factor * duration
      print *, 'signed_duration = ', signed_duration
      datetime_string = date_string // PART_DELIM // time_string !wdb fixme deleteme 
      print *, 'datetime string: ' // datetime_string !wdb fixme deleteme 
!      call convert_NetCDF_DateTimeString_to_ESMF_Time(date_string // PART_DELIM // time_string, start_time, _RC)
      call convert_NetCDF_DateTimeString_to_ESMF_Time(datetime_string, start_time, _RC) !wdb fixme deleteme 
      call ESMF_TimePrint(start_time, options='string', _RC) !wdb fixme deleteme 
      call make_ESMF_TimeInterval(signed_duration, units, start_time, interval, _RC)
      call ESMF_TimeIntervalPrint(interval, options='string', _RC) !wdb fixme deleteme 

      time = start_time + interval
      call ESMF_TimePrint(time, options='string', _RC) !wdb fixme deleteme 

      _RETURN(_SUCCESS)

   end subroutine convert_NetCDF_DateTime_to_ESMF_Time_real

!======================= END NEW HIGH-LEVEL PROCEDURES =========================
!===============================================================================
!========================= NEW LOWER-LEVEL PROCEDURES ==========================

   ! Convert NetCDF datetime to ESMF_Time
   subroutine convert_NetCDF_DateTimeString_to_ESMF_Time(datetime_string, datetime, unusable, rc)
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
!            call ESMF_TimeIntervalSet(interval, startTime=start_time, yy=span, _RC)
         case('months')
            _FAIL('Real values for months are not supported.')
!            call ESMF_TimeIntervalSet(interval, startTime=start_time, mm=span, _RC)
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

   function is_valid_netcdf_datetime_string(string) result(tval)
      character(len=*), parameter :: DIGITS = '0123456789'
      character(len=*), intent(in) :: string
      logical :: tval
      integer :: i

      tval = .false.
      
      if(len(trim(string)) /= len(NETCDF_DATETIME)) return

      do i=1, len_trim(string)
         if(scan(NETCDF_DATETIME(i:i), DIGITS) > 0) then
            if(scan(string(i:i), DIGITS) <= 0) return
         else
            if(string(i:i) /= NETCDF_DATETIME(i:i)) return
         end if
      end do
      
      tval = .true.

   end function is_valid_netcdf_datetime_string

   function is_time_unit(tunit)
      character(len=*), intent(in) :: tunit
      logical :: is_time_unit
      integer :: i

      is_time_unit = .TRUE.
      do i = 1, size(TIME_UNITS)
         if(adjustl(tunit) == adjustl(TIME_UNITS(i))) return
      end do
      is_time_unit = .FALSE.

   end function is_time_unit

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

   ! Split string into all substrings based on delimiter
   function split_all_recursive(string, delimiter, recurse) result(parts)
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: delimiter
      logical , intent(in) :: recurse
      character(len=:), allocatable :: parts(:)

      if(recurse) then
         parts = splitter(trim(string), delimiter)
         return
      end if

      parts = split_all_iterative(string, delimiter)
   contains

      recursive function splitter(string, delimiter) result(parts)
         character(len=*), intent(in) :: string
         character(len=*), intent(in) :: delimiter
         character(len=:), allocatable :: parts(:)
         character(len=:), allocatable :: head
         character(len=:), allocatable :: tail(:)
         integer :: next, last

         last = index(string, delimiter) - 1

         if(last < 0) then
            parts = [string]
         else
            head = string(1:last)
            next = last + len(delimiter) + 1
            tail = splitter(string(next:len(string)), delimiter)
            parts = [head, tail]
         end if

      end function splitter

   end function split_all_recursive

   ! Convert string representing an integer to the integer
   subroutine convert_to_integer(string_in, int_out, rc)
      character(len=*), intent(in) :: string_in
      integer, intent(out) :: int_out
      integer, optional, intent(out) :: rc
      integer :: stat

      read(string_in, '(I16)', iostat=stat) int_out

      if(present(rc)) rc = stat

   end subroutine convert_to_integer

!=========================== END UTILITY PROCEDURES ============================
!===============================================================================

end module MAPL_NetCDF
!   function split_chararray(chararray, delimiter) result(parts)
!      character(len=*), intent(in) :: chararray(:)
!      character(len=*), intent(in) :: delimiter
!      character(len=:), allocatable :: parts(:)
!
!      if(size(chararray) == 0) then
!         parts = chararray
!         return
!      end if
!
!      parts = strip_empty([chararray(1:size(chararray)), split(chararray(size(chararray)), delimiter)])
!
!   end function split_chararray
!
!   function strip_empty(chararray) result(stripped)
!      character(len=*), intent(in) :: chararray
!      character(len=:), allocatable:: stripped
!      integer :: i
!
!      stripped = [character::]
!
!      do i = 1, size(chararray)
!         if(len(chararray(i) > 0)) stripped = [stripped, chararray(i)]
!      end do
!      
!   end function strip_empty

