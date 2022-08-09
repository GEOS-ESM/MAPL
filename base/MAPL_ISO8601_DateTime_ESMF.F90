! ISO 8601 Date/Time MAPL-ESMF Interface
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ISO8601_DateTime_ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ISO8601_DateTime
   use ESMF
   implicit none

   public :: convert_ISO8601_to_esmf_time
   public :: convert_ISO8601_to_esmf_timeinterval

contains

   function convert_ISO8601_to_esmf_time(isostring, rc) result(time)
      character(len=*), intent(in) :: isostring
      integer, optional, intent(out) :: rc
      type(ESMF_Time) :: time
      type(ISO8601DateTime) :: datetime
      integer :: status

! rc :: inout => out
      datetime = ISO8601DateTime(isostring, _RC)

      call ESMF_TimeSet(time,yy = datetime%get_year(), mm = datetime%get_month(), &
         dd = datetime%get_day(), h = datetime%get_hour(), m = datetime%get_minute(), &
         s= datetime%get_second(), _RC)

      _RETURN(_SUCCESS)

   end function convert_ISO8601_to_esmf_time

   function convert_ISO8601_to_esmf_timeinterval(isostring, rc) result(interval)
      character(len=*), intent(in) :: isostring
      integer, optional, intent(out) :: rc
      type(ESMF_TimeInterval) :: interval
      type(ISO8601Duration) :: duration
      integer :: status

      duration = ISO8601Duration(isostring, 0, 1, _RC)

      call ESMF_TimeIntervalSet(interval, yy=duration%get_years(), &
         mm=duration%get_months(), d=duration%get_days(), &
         h=duration%get_hours(), m=duration%get_minutes(), &
         s=duration%get_seconds(), _RC)

      _RETURN(_SUCCESS)
   end function convert_ISO8601_to_esmf_timeinterval

end module MAPL_ISO8601_DateTime_ESMF
