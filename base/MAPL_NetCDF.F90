#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
! Procedures to convert from NetCDF datetime to ESMF_Time and ESMF_TimeInterval
! NetCDF datetime is: {integer, character(len=*)}
! {1800, 'seconds since 2010-01-23 18:30:37'}
! {TIME_SPAN, 'TIME_UNIT since YYYY-MM-DD hh:mm:ss'}
module MAPL_NetCDF

   use, intrinsic :: iso_fortran_env, only: R64 => real64
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use MAPL_DateTime_Parsing, only: datetime_duration
   use MAPL_DateTime_Parsing_ESMF, only: set_ESMF_TimeInterval, set_ESMF_Time_from_ISO8601
   use MAPL_CF_Time, only: CF_Time, convert_CF_Time_to_datetime_duration, &
      extract_ISO8601_from_CF_Time, extract_CF_Time_unit
   use ESMF, only: ESMF_Time, ESMF_Time

   implicit none

   public :: convert_NetCDF_DateTime_to_ESMF

   private

   interface convert_NetCDF_DateTime_to_ESMF
      module procedure :: convert_NetCDF_DateTime_to_ESMF_integer
      module procedure :: convert_NetCDF_DateTime_to_ESMF_real
   end interface convert_NetCDF_DateTime_to_ESMF

   integer, parameter :: MAX_CHARACTER_LENGTH = 64

contains

!===============================================================================
!========================= HIGH-LEVEL PROCEDURES ===========================

   ! Convert NetCDF_DateTime {int_time, units_string} to
   ! ESMF time variables {interval, time0, time1} and time unit {tunit}
   ! time0 is the start time, and time1 is time0 + interval
   subroutine convert_NetCDF_DateTime_to_ESMF_integer(duration, units_string, &
      interval, time0, unusable, time1, tunit, rc)
      integer, intent(in) :: duration
      character(len=*), intent(in) :: units_string
      type(ESMF_TimeInterval), intent(inout) :: interval
      type(ESMF_Time), intent(inout) :: time0
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(inout) :: time1
      character(len=:), allocatable, optional, intent(out) :: tunit
      integer, optional, intent(out) :: rc

      class(CF_Time) :: cft
      class(datetime_duration) :: dt_duration
      character(len=MAX_CHARACTER_LENGTH) :: isostring
      character(len=MAX_CHARACTER_LENGTH) :: tunit_
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      _ASSERT(duration >= 0, 'Negative span not supported')
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units empty')

      cft = CF_Time(duration, units_string)
      call convert_CF_Time_to_datetime_duration(cft, dt_duration, _RC)
      call set_ESMF_TimeInterval(interval, cft, _RC)

      call extract_ISO8601_from_CF_Time(cft, isostring, _RC)
      call set_ESMF_Time_from_ISO8601(time0, isostring, _RC)

      if(present(time1)) time1 = time0 + interval

      if(present(tunit)) then
         call extract_CF_Time_unit(cft, tunit_, _RC)
         tunit = trim(tunit_)
      end if

      _RETURN(_SUCCESS)

   end subroutine convert_NetCDF_DateTime_to_ESMF_integer

   ! Convert NetCDF_DateTime {real_time, units_string} to
   ! ESMF time variables {interval, time0, time1} and time unit {tunit}
   ! time0 is the start time, and time1 is time0 + interval
   subroutine convert_NetCDF_DateTime_to_ESMF_real(duration, units_string, &
      interval, time0, unusable, time1, tunit, rc)
      real(kind=R64), intent(in) :: duration
      character(len=*), intent(in) :: units_string
      type(ESMF_TimeInterval), intent(inout) :: interval
      type(ESMF_Time), intent(inout) :: time0
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(inout) :: time1
      character(len=:), allocatable, optional, intent(out) :: tunit
      integer, optional, intent(out) :: rc

      class(CF_Time) :: cft
      class(datetime_duration) :: dt_duration
      character(len=MAX_CHARACTER_LENGTH) :: isostring
      character(len=MAX_CHARACTER_LENGTH) :: tunit_
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      _ASSERT(duration >= 0, 'Negative span not supported')
      _ASSERT((len_trim(adjustl(units_string)) > 0), 'units empty')

      cft = CF_Time(duration, units_string)
      call convert_CF_Time_to_datetime_duration(cft, dt_duration, _RC)
      call set_ESMF_TimeInterval(interval, cft, _RC)

      call extract_ISO8601_from_CF_Time(cft, isostring, _RC)
      call set_ESMF_Time_from_ISO8601(time0, isostring, _RC)

      if(present(time1)) time1 = time0 + interval

      if(present(tunit)) then
         call extract_CF_Time_unit(cft, tunit_, _RC)
         tunit = trim(tunit_)
      end if

      _RETURN(_SUCCESS)

   end subroutine convert_NetCDF_DateTime_to_ESMF_real

!======================= END HIGH-LEVEL PROCEDURES =========================
!===============================================================================

end module MAPL_NetCDF
