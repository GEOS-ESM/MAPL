module MAPL_DateTimeParsing_ESMF

   use MAPL_DateTimeParsing

   implicit none

contains

   subroutine set_ESMF_TimeInterval_from_datetime_duration(interval, duration, rc)
      type(ESMF_TimeInterval), intent(inout) :: interval
      class(datetime_duration), intent(in) :: duration
      integer, optional, intent(out) :: rc
      integer :: status

      ! Get duration(s) from datetime_duration

      ! Set ESMF_TimeInterval
      if(this % year_is_set) call ESMF_TimeIntervalSet(interval, yy = this % year, _RC)
      if(this % month_is_set) call ESMF_TimeIntervalSet(interval, yy = this % month, _RC)
      if(this % day_is_set) call ESMF_TimeIntervalSet(interval, yy = this % day, _RC)

      if(this % hour_is_real) then
         call ESMF_TimeIntervalSet(interval, h_r8 = this % hour_real, _RC)
      else if(this % hour_is_set) then
         call ESMF_TimeIntervalSet(interval, h = this % hour, _RC)
      end if
         
      if(this % minute_is_real) then
         call ESMF_TimeIntervalSet(interval, m_r8 = this % minute_real, _RC)
      else if(this % minute_is_set) then
         call ESMF_TimeIntervalSet(interval, m = this % minute, _RC)
      end if

      if(this % second_is_real) then
         call ESMF_TimeIntervalSet(interval, s_r8 = this % second_real, _RC)
      else if(this % second_is_set) then
         call ESMF_TimeIntervalSet(interval, s = this % second, _RC)
      end if

      _RETURN(_SUCCESS)

   end subroutine set_ESMF_TimeInterval_from_datetime_duration

   subroutine set_ESMF_Time_from_ISO8601(time, isostring, rc)
       type(ESMF_Time), intent(inout) :: time
       character(len=*), intent(in) :: isostring
       integer, optional, intent(out) :: rc
       integer :: status

       call ESMF_TimeSet(time, isostring, _RC)

       _RETURN(_SUCCESS)

   end subroutine set_ESMF_Time_from_ISO8601
   
end module MAPL_DateTimeParsing_ESMF
