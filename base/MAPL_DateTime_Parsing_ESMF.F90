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
         call ESMF_TimeIntervalSet(interval, yy = duration % year, _rc)
      end if

      if(duration % month_is_set()) then
         call ESMF_TimeIntervalSet(interval, yy = duration % month, _rc)
      end if

      if(duration % day_is_set()) then
         call ESMF_TimeIntervalSet(interval, yy = duration % day, _rc)
      end if

      if(duration % hour_is_real()) then
         call ESMF_TimeIntervalSet(interval, h_r8 = duration % hour_real, _rc)
      else if(duration % hour_is_set()) then
         call ESMF_TimeIntervalSet(interval, h = duration % hour, _rc)
      end if
         
      if(duration % minute_is_real()) then
         call ESMF_TimeIntervalSet(interval, m_r8 = duration % minute_real, _rc)
      else if(duration % minute_is_set()) then
         call ESMF_TimeIntervalSet(interval, m = duration % minute, _rc)
      end if

      if(duration % second_is_real()) then
         call ESMF_TimeIntervalSet(interval, s_r8 = duration % second_real, _rc)
      else if(duration % second_is_set()) then
         call ESMF_TimeIntervalSet(interval, s = duration % second, _rc)
      end if

      _return(_success)

   end subroutine set_ESMF_TimeInterval_from_datetime_duration

   subroutine set_ESMF_Time_from_ISO8601(time, isostring, rc)
       type(ESMF_Time), intent(inout) :: time
       character(len=*), intent(in) :: isostring
       integer, optional, intent(out) :: rc
       integer :: status

       call ESMF_TimeSet(time, isostring, _rc)

       _return(_success)

   end subroutine set_ESMF_Time_from_ISO8601
   
end module MAPL_DateTime_Parsing_ESMF
