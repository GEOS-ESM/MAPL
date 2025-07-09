! ISO 8601 Date/Time MAPL-ESMF Interface
! See MAPL_ISO8601_DateTime for the string parsing and intermediate
! derived types used by these procedures.
#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ISO8601_DateTime_ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ISO8601_DateTime
   use MAPL_DateTime_Parsing
   use ESMF
   implicit none

   public :: convert_ISO8601_to_esmf_time
   public :: convert_ISO8601_to_esmf_timeinterval

contains

   ! Convert an ISO 8601 Time string into an ESMF_Time
   function convert_ISO8601_to_esmf_time(isostring, rc) result(time)
      character(len=*), intent(in) :: isostring
      integer, optional, intent(out) :: rc
      type(ESMF_Time) :: time
      type(ISO8601DateTime) :: datetime
      integer :: status

      datetime = ISO8601DateTime(isostring, _rc)

      call ESMF_TimeSet(time,yy = datetime%get_year(), mm = datetime%get_month(), &
         dd = datetime%get_day(), h = datetime%get_hour(), m = datetime%get_minute(), &
         s= datetime%get_second(), _rc)

      _return(_success)

   end function convert_ISO8601_to_esmf_time

   ! Convert an ISO 8601 Duration string into an ESMF_TimeInterval
   function convert_ISO8601_to_esmf_timeinterval(isostring, rc) result(interval)
      character(len=*), intent(in) :: isostring
      integer, optional, intent(out) :: rc
      type(ESMF_TimeInterval) :: interval
      type(ISO8601Duration) :: duration
      integer :: status

      duration = ISO8601Duration(isostring, 0, 1, _rc)

      call ESMF_TimeIntervalSet(interval, yy=duration%get_years(), &
         mm=duration%get_months(), d=duration%get_days(), &
         h=duration%get_hours(), m=duration%get_minutes(), &
         s=duration%get_seconds(), _rc)

      _return(_success)
   end function convert_ISO8601_to_esmf_timeinterval

end module MAPL_ISO8601_DateTime_ESMF
