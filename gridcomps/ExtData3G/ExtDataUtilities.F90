#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_ExtDataUtilities
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public in_range
   public swap_year
   public shift_year

   contains

   logical function in_range(t1, t2, t0, open_end)
      type(ESMF_Time), intent(in) :: t1
      type(ESMF_Time), intent(in) :: t2
      type(ESMF_Time), intent(in) :: t0
      logical, optional, intent(in) :: open_end

      logical usable_open_end
      usable_open_end=.false.
      if (present(open_end)) usable_open_end = open_end
      if (usable_open_end) then
         in_range = (t0 >= t1) .and. (t0 <= t2)
      else
         in_range = (t0 >= t1) .and. (t0 < t2)
      end if
   end function in_range

   subroutine swap_year(time,year,rc)
      type(ESMF_Time), intent(inout) :: time
      integer, intent(in) :: year
      integer, optional, intent(out) :: rc
      logical :: is_leap_year
      type(ESMF_Calendar) :: calendar
      integer :: status, month, day, hour, minute, second

      is_leap_year=.false.
      call ESMF_TimeGet(time,mm=month,dd=day,h=hour,m=minute,s=second,calendar=calendar,_RC)
      if (day==29 .and. month==2) then
         is_leap_year = ESMF_CalendarIsLeapYear(calendar,year,_RC)
         if (.not.is_leap_year) day=28
      end if
      call ESMF_TimeSet(time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
      _RETURN(_SUCCESS)
  end subroutine

  subroutine shift_year(time, shift, rc)
     type(ESMF_Time), intent(inout) :: time
     integer, intent(in) :: shift
      integer, optional, intent(out) :: rc

      logical :: is_leap_year
      type(ESMF_Calendar) :: calendar
      integer :: status, year, month, day, hour, minute, second, new_year

      is_leap_year=.false.
      call ESMF_TimeGet(time,yy=year, mm=month,dd=day,h=hour,m=minute,s=second,calendar=calendar,_RC)
      new_year=year+shift
      if (day==29 .and. month==2) then
         is_leap_year = ESMF_CalendarIsLeapYear(calendar,year,_RC)
         if (.not.is_leap_year) day=28
      end if
      call ESMF_TimeSet(time,yy=new_year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
      _RETURN(_SUCCESS)
  end subroutine
     

end module
