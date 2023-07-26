#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_CF_Time

   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_DateTime_Parsing

   implicit none

! Comment to test all procedures
   private


! PUBLIC PROCEDURES (ACCESS):
   public :: extract_ISO8601_from_CF_Time
   public :: extract_CF_Time_duration
   public :: extract_CF_Time_units
   public :: convert_CF_Time_to_datetime_duration
! Convert ISO8601 datetime string to CF_Time_base_datetime
   public :: convert_ISO8601_to_CF_Time_base_datetime


! PUBLIC PROCEDURES (INTERFACES):

! Extract an ISO8601 datetime string from the base datetime string in a CF_Time. 
   interface extract_ISO8601_from_CF_Time
      module procedure :: extract_ISO8601_from_CF_Time_units
      module procedure :: extract_ISO8601_from_CF_Time_cf_time
   end interface

! Extract the duration of a CF Time.
   interface extract_CF_Time_duration
      module procedure :: extract_CF_Time_duration_cf_time_real
      module procedure :: extract_CF_Time_duration_cf_time_integer
   end interface extract_CF_Time_duration

! Extract the time units from a CF Time.
   interface extract_CF_Time_unit
      module procedure :: extract_CF_Time_unit_cf_time
      module procedure :: extract_CF_Time_unit_units
   end interface extract_CF_Time_unit

! Extract datetime_duration from CF Time.
   interface convert_CF_Time_to_datetime_duration
      module procedure :: convert_CF_Time_to_datetime_duration_integer
      module procedure :: convert_CF_Time_to_datetime_duration_real
      module procedure :: convert_CF_Time_to_datetime_duration_integer_duration
      module procedure :: convert_CF_Time_to_datetime_duration_real_duration
   end interface convert_CF_Time_to_datetime_duration


! PRIVATE INTERFACES:

   interface split
      module procedure :: split_characters
   end interface split


! TYPES (DEFINITIONS):

! CF_TIME: derived type to hold the data for CF Time values
   type, abstract :: CF_Time
      public
      logical :: is_valid
      character(len=:), allocatable :: time_unit
      character(len=:), allocatable :: base_datetime
   contains
      procedure, public, pass(this) :: check => check_cf_time
   end type CF_Time

   type, extends(CF_Time) :: CF_Time_Integer
      public
      integer :: duration
   end type CF_Time_Integer

   type, extends(CF_Time) :: CF_Time_Real
      public
      real(kind=R64) :: duration
   end type CF_Time_Real
   
   interface CF_Time
      module procedure :: construct_cf_time_integer
      module procedure :: construct_cf_time_real
   end interface CF_Time

! END CF_TIME 


! CONSTANTS:
   integer, parameter :: MAX_CHARACTER_LENGTH = 64
   character, parameter :: DATE_DELIM = '-'
   character, parameter :: TIME_DELIM = ':'
   character, parameter :: ISO_DELIM = 'T'
   character(len=2), parameter :: CF_DELIM = ' ' // ISO_DELIM
   character(len=*), parameter = EMPTY_STRING = ''


contains


! PUBLIC PROCEDURES (DEFINITION):

   subroutine extract_ISO8601_from_CF_Time_units(units, isostring, rc)
      character(len=*), intent(in) :: units
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: isostring
      integer, optional, intent(out) :: rc
      type(CF_Time_Integer) :: cft
      integer :: status
      
      call extract_ISO8601_from_CF_Time(CF_Time(0, units), isostring, _RC)

      _RETURN(_SUCCESS)

   end subroutine extract_ISO8601_from_CF_Time_units

   function extract_ISO8601_from_CF_Time_cf_time(cft, isostring, rc)
      class(CF_Time), intent(in) :: cft
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: isostring
      integer, optional, intent(out) :: rc 
      integer :: status

      call cft % check(_RC)
      
      call convert_CF_Time_datetime_string_to_ISO8601(cft % base_datetime, isostring, _RC)

      _RETURN(_SUCCESS)
      
   end function extract_ISO8601_from_CF_Time_cf_time
   
   subroutine extract_CF_Time_duration_cf_time_real(cft, duration, rc)
      class(CF_Time_Real), intent(in) :: cft
      real(kind=R64), intent(out) :: duration
      integer, optional, intent(out) :: rc
      integer :: status
      
      call cft % check(_RC)

      duration = cft % duration

      _RETURN(_SUCCESS)

   end subroutine extract_CF_Time_duration_cf_time_real

   subroutine extract_CF_Time_duration_cf_time_integer(cft, duration, rc)
      class(CF_Time_Integer), intent(in) :: cft
      integer, intent(out) :: duration
      integer, optional, intent(out) :: rc
      integer :: status
      
      call cft % check(_RC)

      duration = cft % duration

      _RETURN(_SUCCESS)
      
   end subroutine extract_CF_Time_duration_cf_time_integer
   
   subroutine extract_CF_Time_unit_cf_time(cft, time_unit, rc) 
      class(CF_Time), intent(in) :: cft
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: time_unit
      integer, optional, intent(out) :: rc

      call cft % check(_RC)

      time_units = cft % time_unit

      _RETURN(_SUCCESS)

   end subroutine extract_CF_Time_unit_cf_time

   subroutine extract_CF_Time_unit_units(units, time_unit, rc)
      character(len=*), intent(in) :: units
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: time_unit
      integer, optional, intent(out) :: rc

      call extract_CF_Time_units(CF_Time(0, units), time_units, _RC)

      _RETURN(_SUCCESS)

   end subroutine extract_CF_Time_unit_units

   subroutine convert_ISO8601_to_CF_Time_datestring(isostring, datestring, rc)
      character(len=*), intent(in) :: isostring
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: datestring
      integer, optional, intent(out) :: rc
      
      datestring = remove_zero_pad(isostring)

      _RETURN(_SUCCESS)
      
   end subroutine convert_ISO8601_to_CF_Time_datestring

   subroutine convert_CF_Time_to_datetime_duration_integer(cft, dt_duration, rc)
      class(CF_Time_Integer), intent(in) :: cft
      type(datetime_duration), intent(out) :: dt_duration
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind(TIME_UNIT)) :: tu

      call cft % check(_RC)

      tu = time_unit(cft % time_units())
      if(tu == TIME_UNIT_UNKNOWN) then
         _FAIL('Unrecognized time unit in CF Time')
      endif

      call dt_duration % set_value(tu, cft % duration)

      _RETURN(_SUCCESS)

   end subroutine convert_CF_Time_to_datetime_duration_integer
   
   subroutine convert_CF_Time_to_datetime_duration_real(cft, dt_duration, rc)
      class(CF_Time_Real), intent(in) :: cft
      type(datetime_duration), intent(out) :: dt_duration
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind(TIME_UNIT)) :: tu

      call cft % check(_RC)

      tu = time_unit(cft % time_units())
      if(tu == TIME_UNIT_UNKNOWN) then
         _FAIL('Unrecognized time unit in CF Time')
      endif

      call dt_duration % set_value(tu, cft % duration)

      _RETURN(_SUCCESS)

   end subroutine convert_CF_Time_to_datetime_duration_real

   subroutine convert_CF_Time_to_datetime_duration_integer_duration(duration, units, dt_duration, rc)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units
      type(datetime_duration), intent(out) :: dt_duration
      integer, optional, intent(out) :: rc
      integer :: status

      call convert_CF_Time_to_datetime_duration(CF_Time(duration, units), dt_duration, _RC)

      _RETURN(_SUCCESS)

   end subroutine convert_CF_Time_to_datetime_duration_integer_duration
   
   subroutine convert_CF_Time_to_datetime_duration_real_duration(duration, units, dt_duration, rc)
      real(kind=R64), intent(in) :: duration
      character(len=*), intent(in) :: units
      type(datetime_duration), intent(out) :: dt_duration
      integer, optional, intent(out) :: rc
      integer :: status

      call convert_CF_Time_to_datetime_duration(CF_Time(duration, units), dt_duration, _RC)

      _RETURN(_SUCCESS)

   end subroutine convert_CF_Time_to_datetime_duration_real_duration
   
   function convert_CF_Time_datetime_string_to_ISO8601(datetime_string) result(isodatetime)
      character(len=*), intent(in) :: datetime_string
      character(len=MAX_CHARACTER_LENGTH) :: isodatetime
      character(len=MAX_CHARACTER_LENGTH) :: remainder
      ! parts [year, month, day, hour, minute, second)
      character(len=MAX_CHARACTER_LENGTH) :: part(NUM_CF_TIME_UNITS)
      character(len=MAX_CHARACTER_LENGTH) :: delimiters(NUM_CF_TIME_UNITS)

      datetime = EMPTY_STRING
      remainder = datetime_string

      call split(trim(remainder), part(YEAR), remainder, DATE_DELIM)
      call split(trim(remainder), part(MONTH), remainder, DATE_DELIM)
      call split(trim(remainder), part(DAY), remainder, CF_DELIM)
      call split(trim(remainder), part(HOUR), remainder, TIME_DELIM)
      call split(trim(remainder), part(MINUTE), remainder, TIME_DELIM)
      part(SECOND) = trim(remainder) 

      call update_datetime(datetime, part(YEAR), 4, DATE_DELIM)
      call update_datetime(datetime, part(MONTH), 2, DATE_DELIM)
      call update_datetime(datetime, part(DAY), 2, ISO_DELIM)
      call update_datetime(datetime, part(HOUR), 2, TIME_DELIM)
      call update_datetime(datetime, part(MINUTE), 2, TIME_DELIM)
      call update_datetime(datetime, part(SECOND), 2)
      
   contains

      subroutine update_datetime(datetime_, text, width, delm)
         character(len=MAX_CHARACTER_LENGTH), intent(inout) :: datetime_
         character(len=*), intent(in) :: text
         integer, optional, intent(in) :: width
         character(len=*), optional, intent(in) :: delm
         character(len=MAX_CHARACTER_LENGTH) :: text_
         
         text_ = text
         if(present(width)) text_ = zero_pad(text, width)
         datetime_ = trim(datetime_) // trim(text_)
         if(present(delm)) datetime_ = trim(datetime_) // trim(delm)

      end subroutine update_datetime

   end function convert_CF_Time_datetime_string_to_ISO8601

! END PUBLIC PROCEDURES (DEFINITION)


! CONSTRUCTORS:

! CF_TIME (CONSTRUCTORS):

   function construct_cf_time_integer(duration, units) result (cft)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units
      type(CF_Time) :: cft
      integer :: status
      
      if(duration < 0) return

      call cft % initialize_cf_time(units, rc=status)
      
      cft % duration = duration

      cft % valid = status

   end function construct_cf_time_integer

   function construct_cf_time_real(duration, units) result (cft)
      real(kind=R64), intent(in) :: duration
      character(len=*), intent(in) :: units
      type(CF_Time) :: cft
      integer :: status
      
      if(duration < 0) return

      call cft % initialize_cf_time(units, rc=status)
      
      cft % duration = duration

      cft % valid = status

   end function construct_cf_time_real

   subroutine initialize_cf_time(this, units, rc)
      class(CF_time), intent(inout) :: this
      character(len=*), intent(in) :: units
      integer, optional, intent(out) :: rc
      character(len=MAX_CHARACTER_LENGTH) :: token(2), remainder
      integer :: i
      
      if(present(rc)) rc = _FAILURE

      remainder = units

      do i = 1, size(token)
         if(len_trim(remainder) == 0) return
         call split(trim(remainder), token(i), remainder, CF_DELIM)
      end do

      cft % time_unit = token(1)
      cft % base_datetime = token(3)

      if(present(rc)) rc = _SUCCESS

   end subroutine initialize_cf_time

   subroutine check_cf_time(this, rc)
      class(CF_Time), intent(in) :: this
      integer, optional, intent(out) :: rc 
      integer :: status

      if(.not. this % is_valid) then
         _FAIL("Invalid CF_Time")
      end if

   end subroutine check_cf_time

! END CONSTRUCTORS


! UTILITY PROCEDURES:

! ZERO_PAD - UTILITY
   function zero_pad(number_string, width) result(padded)
      character(len=*), intent(in) :: number_string
      integer, intent(in) :: width
      character(len=MAX_CHARACTER_LENGTH) :: padded
      integer :: num_zeros

      num_zeros = width - len_trim(number_string)
      if(num_zeros > 0) then
         padded = repeat('0', num_zeros) // number_string
      else
         padded = number_string
      end if

   end function zero_pad

! SPLITTER - UTILITY
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
      
! REMOVE_ZERO_PAD - UTILITY
   function remove_zero_pad(s) result(u)
      character(len=*), intent(in) :: s
      character(len=len(string)) :: u
      character :: c
      integer :: i, n
      logical :: follows(len(s))
      integer, allocatable :: indices

      indices = .not. findloc((follows_digit(s) .and. (s == '0')), .TRUE.)
      u = s(indices)
   end function remove_zero_pad

! FOLLOWS_DIGIT - UTILITY
   function follows_digit(s) result(follows)
      character(len=*), intent(in) :: s
      logical :: follows(len(s))
      
      follows(1) = .FALSE.
      follows(2:) = is_digit(1:(len(s)-1))

   end function follows_digit
   
end module MAPL_CF_Time
