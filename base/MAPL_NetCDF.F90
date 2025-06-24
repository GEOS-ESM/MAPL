#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
! Procedures to convert from NetCDF datetime to ESMF_Time and ESMF_TimeInterval
! NetCDF datetime is: {integer, character(len=*)}
! {1800, 'seconds since 2010-01-23 18:30:37'}
! {TIME_SPAN, 'TIME_UNIT since YYYY-MM-DD hh:mm:ss'}
module MAPL_NetCDF

   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use MAPL_DateTime_Parsing, only: datetime_duration
   use MAPL_DateTime_Parsing_ESMF
   use MAPL_CF_Time

   implicit none

   public :: convert_NetCDF_DateTime_to_ESMF
   public :: get_ESMF_Time_from_NetCDF_DateTime

   private

   interface convert_NetCDF_DateTime_to_ESMF
      module procedure :: get_ESMF_Time_from_NetCDF_DateTime_integer
      module procedure :: get_ESMF_Time_from_NetCDF_DateTime_real
   end interface convert_NetCDF_DateTime_to_ESMF

   interface get_ESMF_Time_from_NetCDF_DateTime
      module procedure :: get_ESMF_Time_from_NetCDF_DateTime_integer
      module procedure :: get_ESMF_Time_from_NetCDF_DateTime_real
   end interface get_ESMF_Time_from_NetCDF_DateTime

!   integer, parameter :: MAX_CHARACTER_LENGTH = 64

contains

!===============================================================================
!========================= HIGH-LEVEL PROCEDURES ===========================

   ! Convert NetCDF_DateTime {int_time, units_string} to
   ! ESMF time variables {interval, basetime, time} and time unit {time_unit}
   ! basetime is the start time, and time is basetime + interval
   subroutine get_ESMF_Time_from_NetCDF_DateTime_integer(duration, units_string, &
      interval, basetime, unusable, time, time_unit, rc)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units_string
      type(ESMF_TimeInterval), intent(inout) :: interval
      type(ESMF_Time), intent(inout) :: basetime
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(inout) :: time
      character(len=:), allocatable, optional, intent(out) :: time_unit
      integer, optional, intent(out) :: rc

      type(CF_Time_Integer) :: cft
      type(datetime_duration) :: dt_duration
      character(len=MAX_CHARACTER_LENGTH) :: isostring
      character(len=MAX_CHARACTER_LENGTH) :: tunit_
      integer :: status
      
      __UNUSED_DUMMY(unusable)

      __ASSERT((len_trim(adjustl(units_string)) > 0), 'units empty')

      cft = CF_Time_Integer(duration, units_string)
      call convert_CF_Time_to_datetime_duration(cft, dt_duration, __RC)
      call set_ESMF_TimeInterval(interval, dt_duration, __RC)

      call extract_ISO8601_from_CF_Time(cft, isostring, __RC)
      call set_ESMF_Time_from_ISO8601(basetime, isostring, __RC)

      if(present(time)) time = basetime + interval

      if(present(time_unit)) then
         call extract_CF_Time_unit(cft, tunit_, __RC)
         time_unit = trim(tunit_)
      end if

      __RETURN(__SUCCESS)

   end subroutine get_ESMF_Time_from_NetCDF_DateTime_integer

   ! Convert NetCDF_DateTime {real_time, units_string} to
   ! ESMF time variables {interval, basetime, time} and time unit {time_unit}
   ! basetime is the start time, and time is basetime + interval
   subroutine get_ESMF_Time_from_NetCDF_DateTime_real(duration, units_string, &
      interval, basetime, unusable, time, time_unit, rc)
      real(kind=ESMF_KIND_R8), intent(in) :: duration
      character(len=*), intent(in) :: units_string
      type(ESMF_TimeInterval), intent(inout) :: interval
      type(ESMF_Time), intent(inout) :: basetime
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(inout) :: time
      character(len=:), allocatable, optional, intent(out) :: time_unit
      integer, optional, intent(out) :: rc

      type(CF_Time_Real) :: cft
      type(datetime_duration) :: dt_duration
      character(len=MAX_CHARACTER_LENGTH) :: isostring
      character(len=MAX_CHARACTER_LENGTH) :: tunit_
      integer :: status
      
      __UNUSED_DUMMY(unusable)

      __ASSERT((len_trim(adjustl(units_string)) > 0), 'units empty')

      cft = CF_Time_Real(duration, units_string)
      call convert_CF_Time_to_datetime_duration(cft, dt_duration, __RC)
      call set_ESMF_TimeInterval(interval, dt_duration, __RC)

      call extract_ISO8601_from_CF_Time(cft, isostring, __RC)
      call set_ESMF_Time_from_ISO8601(basetime, isostring, __RC)

      if(present(time)) time = basetime + interval

      if(present(time_unit)) then
         call extract_CF_Time_unit(cft, tunit_, __RC)
         time_unit = trim(tunit_)
      end if

      __RETURN(__SUCCESS)

   end subroutine get_ESMF_Time_from_NetCDF_DateTime_real

!======================= END HIGH-LEVEL PROCEDURES =========================
!===============================================================================

end module MAPL_NetCDF
