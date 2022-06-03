#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_AlarmUtils
  use ESMF
  use MAPL_ExceptionHandling
  implicit none
  private

  public :: MAPL_get_previous_ring

contains

  ! Analog of range() procedure in Python for constructing
  ! an arithmetic sequence of values.  Introducing
  ! into MAPL to enforce consistent roundoff within
  ! various parts of the model that generate lat and lon
  ! coordinates.

  function MAPL_get_previous_ring(alarm,rc) result(previous_ring)
     type(ESMF_Time) :: previous_ring
     type(ESMF_Alarm), intent(in) :: alarm
     integer, intent(out), optional :: rc

     integer :: status
     type(ESMF_Time) :: ring_time
     type(ESMF_TimeInterval) :: ring_interval

     call ESMF_AlarmGet(alarm,ringTime=ring_time,ringInterval=ring_interval,_RC)
     previous_ring = ring_time - ring_interval

     _RETURN(_SUCCESS)
        
  end function


end module MAPL_AlarmUtils
