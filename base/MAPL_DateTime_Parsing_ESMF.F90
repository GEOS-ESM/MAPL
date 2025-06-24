#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_DateTime_Parsing_ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_DateTime_Parsing
   use ESMF

   implicit none

   public :: set_ESMF_TimeInterval, set_ESMF_Time_from_ISO8601

   interface set_ESMF_TimeInterval
      module procedure :: set_ESMF_TimeInterval_from_datetime_duration
   end interface set_ESMF_TimeInterval

contains

   subroutine set_ESMF_TimeInterval_from_datetime_duration(interval, duration, rc)
      type(ESMF_TimeInterval), intent(inout) :: interval
      class(datetime_duration), intent(in) :: duration
      integer, optional, intent(out) :: rc
      integer :: status

      ! Get duration(s) from datetime_duration

      ! Set ESMF_TimeInterval

      if(duration % year_is_set()) then
         call ESMF_TimeIntervalSet(interval, yy = duration % year, __RC)
      end if

      if(duration % month_is_set()) then
         call ESMF_TimeIntervalSet(interval, yy = duration % month, __RC)
      end if

      if(duration % day_is_set()) then
         call ESMF_TimeIntervalSet(interval, yy = duration % day, __RC)
      end if

      if(duration % hour_is_real()) then
         call ESMF_TimeIntervalSet(interval, h_r8 = duration % hour_real, __RC)
      else if(duration % hour_is_set()) then
         call ESMF_TimeIntervalSet(interval, h = duration % hour, __RC)
      end if
         
      if(duration % minute_is_real()) then
         call ESMF_TimeIntervalSet(interval, m_r8 = duration % minute_real, __RC)
      else if(duration % minute_is_set()) then
         call ESMF_TimeIntervalSet(interval, m = duration % minute, __RC)
      end if

      if(duration % second_is_real()) then
         call ESMF_TimeIntervalSet(interval, s_r8 = duration % second_real, __RC)
      else if(duration % second_is_set()) then
         call ESMF_TimeIntervalSet(interval, s = duration % second, __RC)
      end if

      __RETURN(__SUCCESS)

   end subroutine set_ESMF_TimeInterval_from_datetime_duration

   subroutine set_ESMF_Time_from_ISO8601(time, isostring, rc)
       type(ESMF_Time), intent(inout) :: time
       character(len=*), intent(in) :: isostring
       integer, optional, intent(out) :: rc
       integer :: status

       call ESMF_TimeSet(time, isostring, __RC)

       __RETURN(__SUCCESS)

   end subroutine set_ESMF_Time_from_ISO8601
   
end module MAPL_DateTime_Parsing_ESMF
