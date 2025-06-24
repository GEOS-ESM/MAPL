#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_CF_Time

   use, intrinsic :: iso_fortran_env, only : R64 => real64
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_DateTime_Parsing

   implicit none

! Comment to test all procedures
   private

! PUBLIC PROCEDURES (ACCESS):
   public :: extract_ISO8601_from_CF_Time
   public :: extract_CF_Time_duration
   public :: extract_CF_Time_unit
   public :: convert_CF_Time_to_datetime_duration
! Convert ISO8601 datetime string to CF_Time_base_datetime
   public :: convert_ISO8601_to_CF_Time_base_datetime
   public :: CF_Time, CF_Time_Integer, CF_Time_Real

   public :: MAX_CHARACTER_LENGTH

! PUBLIC PROCEDURES (INTERFACES):

! Extract an ISO8601 datetime string from the base datetime string in a CF_Time. 
   interface extract_ISO8601_from_CF_Time
      module procedure :: extract_ISO8601_from_CF_Time_units
      module procedure :: extract_ISO8601_from_CF_Time_cf_time
   end interface extract_ISO8601_from_CF_Time

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
      logical :: is_valid
      character(len=:), allocatable :: time_unit
      character(len=:), allocatable :: base_datetime
   end type CF_Time

   type, extends(CF_Time) :: CF_Time_Integer
      integer :: duration
   end type CF_Time_Integer

   type, extends(CF_Time) :: CF_Time_Real
      real(kind=R64) :: duration
   end type CF_Time_Real
   
   interface CF_Time_Integer
      module procedure :: construct_cf_time_integer
   end interface CF_Time_Integer

   interface CF_Time_Real
      module procedure :: construct_cf_time_real
   end interface CF_Time_Real

! END CF_TIME 


! CONSTANTS:
   character, parameter :: DATE_DELIM = '-'
   character, parameter :: TIME_DELIM = ':'
   character, parameter :: ISO_DELIM = 'T'
   character(len=2), parameter :: CF_DELIM = ' ' // ISO_DELIM
   character(len=*), parameter :: EMPTY_STRING = ''
   character, parameter :: DECIMAL_POINT = '.'
   !character(len=*), parameter :: DIGIT_CHARACTERS = '1234567890'

contains


! PUBLIC PROCEDURES (DEFINITION):

   subroutine extract_ISO8601_from_CF_Time_units(units, isostring, rc)
      character(len=*), intent(in) :: units
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: isostring
      integer, optional, intent(out) :: rc
      integer :: status
      
      call extract_ISO8601_from_CF_Time(CF_Time_Integer(0, units), isostring, __RC)

      __RETURN(__SUCCESS)

   end subroutine extract_ISO8601_from_CF_Time_units

   subroutine extract_ISO8601_from_CF_Time_cf_time(cft, isostring, rc)
      class(CF_Time), intent(in) :: cft
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: isostring
      integer, optional, intent(out) :: rc 

      if(cft % is_valid) then
         isostring = convert_CF_Time_datetime_string_to_ISO8601(cft % base_datetime)
         __RETURN(__SUCCESS)
      end if
      
      __RETURN(__FAILURE)

   end subroutine extract_ISO8601_from_CF_Time_cf_time
   
   subroutine extract_CF_Time_duration_cf_time_real(cft, duration, rc)
      class(CF_Time_Real), intent(in) :: cft
      real(kind=R64), intent(out) :: duration
      integer, optional, intent(out) :: rc
      
      if(cft % is_valid) then
         duration = cft % duration
         __RETURN(__SUCCESS)
      end if

      __RETURN(__FAILURE)

   end subroutine extract_CF_Time_duration_cf_time_real

   subroutine extract_CF_Time_duration_cf_time_integer(cft, duration, rc)
      class(CF_Time_Integer), intent(in) :: cft
      integer, intent(out) :: duration
      integer, optional, intent(out) :: rc
      
      if(cft % is_valid) then
         duration = cft % duration
         __RETURN(__SUCCESS)
      end if

      __RETURN(__FAILURE)
      
   end subroutine extract_CF_Time_duration_cf_time_integer
   
   subroutine extract_CF_Time_unit_cf_time(cft, time_unit, rc) 
      class(CF_Time), intent(in) :: cft
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: time_unit
      integer, optional, intent(out) :: rc

      if(cft % is_valid) then
         time_unit = cft % time_unit
         __RETURN(__SUCCESS)
      end if

      __RETURN(__FAILURE)

   end subroutine extract_CF_Time_unit_cf_time

   subroutine extract_CF_Time_unit_units(units, time_unit, rc)
      character(len=*), intent(in) :: units
      character(len=MAX_CHARACTER_LENGTH), intent(out) :: time_unit
      integer, optional, intent(out) :: rc
      integer :: status

      call extract_CF_Time_unit(CF_Time_Integer(0, units), time_unit, __RC)

      __RETURN(__SUCCESS)

   end subroutine extract_CF_Time_unit_units

   subroutine convert_CF_Time_to_datetime_duration_integer(cft, dt_duration, rc)
      class(CF_Time_Integer), intent(in) :: cft
      type(datetime_duration), intent(out) :: dt_duration
      integer, optional, intent(out) :: rc
      integer(kind(TIME_UNIT)) :: tu

      if(.not. cft % is_valid) then
         __RETURN(__FAILURE)
      end if

      tu = get_time_unit(cft % time_unit)
      __ASSERT(tu /= UNKNOWN_TIME_UNIT, 'Unable to find TIME_UNIT ' // cft % time_unit) 

      call dt_duration % set_value(tu, cft % duration)
      
      __RETURN(__SUCCESS)

   end subroutine convert_CF_Time_to_datetime_duration_integer
   
   subroutine convert_CF_Time_to_datetime_duration_real(cft, dt_duration, rc)
      class(CF_Time_Real), intent(in) :: cft
      type(datetime_duration), intent(out) :: dt_duration
      integer, optional, intent(out) :: rc
      integer(kind(TIME_UNIT)) :: tu

      if(.not. cft % is_valid) then
         __RETURN(__FAILURE)
      end if

      tu = get_time_unit(cft % time_unit)
      __ASSERT(tu /= UNKNOWN_TIME_UNIT, 'Unable to find TIME_UNIT ' // cft % time_unit) 

      call dt_duration % set_value(tu, cft % duration)

      __RETURN(__SUCCESS)

   end subroutine convert_CF_Time_to_datetime_duration_real

   subroutine convert_CF_Time_to_datetime_duration_integer_duration(duration, units, dt_duration, rc)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units
      type(datetime_duration), intent(out) :: dt_duration
      integer, optional, intent(out) :: rc
      integer :: status

      call convert_CF_Time_to_datetime_duration(CF_Time_Integer(duration, units), dt_duration, __RC)

      __RETURN(__SUCCESS)

   end subroutine convert_CF_Time_to_datetime_duration_integer_duration
   
   subroutine convert_CF_Time_to_datetime_duration_real_duration(duration, units, dt_duration, rc)
      real(kind=R64), intent(in) :: duration
      character(len=*), intent(in) :: units
      type(datetime_duration), intent(out) :: dt_duration
      integer, optional, intent(out) :: rc
      integer :: status

      call convert_CF_Time_to_datetime_duration(CF_Time_Real(duration, units), dt_duration, __RC)

      __RETURN(__SUCCESS)

   end subroutine convert_CF_Time_to_datetime_duration_real_duration
   
   function convert_CF_Time_datetime_string_to_ISO8601(datetime_string) result(isodatetime)
      character(len=*), intent(in) :: datetime_string
      character(len=MAX_CHARACTER_LENGTH) :: isodatetime
      character(len=MAX_CHARACTER_LENGTH) :: remainder
      character(len=MAX_CHARACTER_LENGTH) :: part(NUM_TIME_UNITS)

      isodatetime = EMPTY_STRING
      remainder = datetime_string

      call split(trim(remainder), part(YEAR_TIME_UNIT), remainder, DATE_DELIM)
      call split(trim(remainder), part(MONTH_TIME_UNIT), remainder, DATE_DELIM)
      call split(trim(remainder), part(DAY_TIME_UNIT), remainder, CF_DELIM)
      call split(trim(remainder), part(HOUR_TIME_UNIT), remainder, TIME_DELIM)
      call split(trim(remainder), part(MINUTE_TIME_UNIT), remainder, TIME_DELIM)
      part(SECOND_TIME_UNIT) = trim(remainder) 

      call update_datetime(isodatetime, part(YEAR_TIME_UNIT), 4, DATE_DELIM)
      call update_datetime(isodatetime, part(MONTH_TIME_UNIT), 2, DATE_DELIM)
      call update_datetime(isodatetime, part(DAY_TIME_UNIT), 2, ISO_DELIM)
      call update_datetime(isodatetime, part(HOUR_TIME_UNIT), 2, TIME_DELIM)
      call update_datetime(isodatetime, part(MINUTE_TIME_UNIT), 2, TIME_DELIM)
      call update_datetime(isodatetime, part(SECOND_TIME_UNIT), 2)
      
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

   function convert_ISO8601_to_CF_Time_base_datetime(isostring) result(base_datetime) 
      character(len=*), intent(in) :: isostring
      character(len=len(isostring)) :: base_datetime

      base_datetime = remove_zero_pad(isostring)
      base_datetime = substitute(base_datetime, 'T', ' ')

   end function convert_ISO8601_to_CF_Time_base_datetime 

! END PUBLIC PROCEDURES (DEFINITION)


! CONSTRUCTORS:

! CF_TIME (CONSTRUCTORS):

   function construct_cf_time_integer(duration, units) result (cft)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units
      type(CF_Time_Integer) :: cft
      
      cft % duration = duration
      call initialize_cf_time(cft, units)

   end function construct_cf_time_integer

   function construct_cf_time_real(duration, units) result (cft)
      real(kind=R64), intent(in) :: duration
      character(len=*), intent(in) :: units
      type(CF_Time_Real) :: cft
      
      cft % duration = duration
      call initialize_cf_time(cft, units)

   end function construct_cf_time_real

   subroutine initialize_cf_time(cft, units)
      class(CF_Time), intent(inout) :: cft
      character(len=*), intent(in) :: units
      character(len=MAX_CHARACTER_LENGTH) :: token, remainder
      
      cft % is_valid = .FALSE.
      remainder = units
      if(len_trim(remainder) == 0) return
      call split(trim(remainder), token, remainder, CF_DELIM)
      cft % time_unit = token
      call split(trim(remainder), token, remainder, CF_DELIM)
      cft % base_datetime = remainder
      cft % is_valid = .TRUE.

   end subroutine initialize_cf_time

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
      
! UTILITIES

   function remove_zero_pad(isostring) result(unpadded)
      character(len=*), intent(in) :: isostring
      character(len=len(isostring)) :: unpadded
      character(len=:), allocatable :: part(:)
      character(len=len(isostring)) :: fraction_part
      integer :: i

      part = get_ISO8601_substrings(isostring)
      fraction_part = get_ISO8601_fractional_seconds(isostring)
      unpadded = trim(part(1))
      do i = 2, size(part)
         part(i) = strip_zero(part(i))
         unpadded = trim(unpadded) // trim(part(i)) 
      end do

      fraction_part = strip_zero(fraction_part, back = .TRUE.)
      if(len_trim(fraction_part) > 0) unpadded = trim(unpadded) // DECIMAL_POINT // trim(fraction_part)

   end function remove_zero_pad
   
   function substitute(string, ch1, ch2) result(replaced)
      character(len=*), intent(in) :: string
      character, intent(in) :: ch1, ch2
      character(len=len(string)) :: replaced
      integer :: i, j

      j = 0
      replaced = string
      i = index(replaced((j+1):), ch1) 
      do while (i > 0)
         j = j + i
         if(j > len(replaced)) exit
         replaced(j:j) = ch2
         if(j == len(replaced)) exit
         i = index(replaced((j+1):), ch1) 
      end do

   end function substitute

   elemental logical function is_zero(ch)
      character, intent(in) :: ch
      is_zero = (ch == '0')
   end function is_zero

   function get_ISO8601_substrings(isostring) result(substring)
      character(len=*), intent(in) :: isostring
      integer, parameter :: NUM_DT_PARTS = 6
      integer, parameter :: DT_PART_WIDTH = 5
      character(len=DT_PART_WIDTH) :: substring(NUM_DT_PARTS)

      substring = EMPTY_STRING

      substring(1) = isostring(1:5)
      substring(2) = isostring(6:8)
      substring(3) = isostring(9:11)
      substring(4) = isostring(12:14)
      substring(5) = isostring(15:17)
      substring(6) = isostring(18:19)
      
   end function get_ISO8601_substrings

   function get_ISO8601_fractional_seconds(isostring) result(fs)
      character(len=*), intent(in) :: isostring
      integer, parameter :: FIRST_INDEX = 20
      character(len=len(isostring)) :: fs
      integer :: i, j

      fs = EMPTY_STRING
      if(len_trim(isostring) < FIRST_INDEX) return
      i = FIRST_INDEX
      if(isostring(i:i) /= DECIMAL_POINT) return
      i = i + 1
      j = verify(isostring(i:), DIGIT_CHARACTERS)
      select case(j)
         case(0)
            fs = isostring(i:)
         case(1)
            return
         case default
            j = j + i - 2
            fs = isostring(i:j)
      end select

   end function get_ISO8601_fractional_seconds

   function strip_zero(string, back) result(stripped)
      character(len=*), intent(in) :: string
      logical, optional, intent(in) :: back
      character(len=len(string)) :: stripped
      logical :: back_
      integer :: i, j, n
      character :: ch

      stripped = EMPTY_STRING
      back_ = .FALSE.
      if(present(back)) back_ = back

      n = len_trim(string)
      if(back_) then
         i = 1
         do j = n, i, -1
            ch = string(j:j)
            if(.not. is_zero(ch)) exit
         end do
      else
         j = n
         do i = 1, n
            ch = string(i:i)
            if(.not. is_zero(ch)) exit
         end do
         i = min(i, j)
      end if

      stripped = string(i:j)

   end function strip_zero

end module MAPL_CF_Time
