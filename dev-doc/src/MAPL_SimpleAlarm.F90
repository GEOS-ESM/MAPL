#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_SimpleAlarm
   use ESMF
   use MAPL_ExceptionHandling

   implicit none
   private

   public SimpleAlarm

   type :: SimpleAlarm
      private
      type(ESMF_Time) :: reference_time
      type(ESMF_TimeInterval) :: ring_interval
      contains
         procedure :: is_ringing
   end type SimpleAlarm

   interface SimpleAlarm
      module procedure new_simple_alarm
   end interface SimpleAlarm

contains

   function new_simple_alarm(reference_time,ring_interval,rc) result(new_alarm)
      type(ESMF_Time), intent(in) :: reference_time
      type(ESMF_TimeInterval), intent(in) :: ring_interval
      integer, optional, intent(out) :: rc

      type(SimpleAlarm) :: new_alarm

      new_alarm%reference_time = reference_time
      new_alarm%ring_interval = ring_interval
 
      _RETURN(_SUCCESS)
   end function new_simple_alarm

   function is_ringing(this,current_time,rc) result(ringing)
      class(SimpleAlarm), intent(inout) :: this
      type(ESMF_Time), intent(inout) :: current_time
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: ringing
      integer(ESMF_KIND_I8) :: ring_interval_i8, elapsed_interval_i8
      type(ESMF_TimeInterval) :: elapsed_interval

      elapsed_interval = current_time - this%reference_time
      call ESMF_TimeIntervalGet(elapsed_interval,s_i8=elapsed_interval_i8,rc=status)
      _VERIFY(status)
      call ESMF_TimeIntervalGet(this%ring_interval,s_i8=ring_interval_i8,rc=status)
      _VERIFY(status)
      if (mod(elapsed_interval_i8,ring_interval_i8) == 0) then
         ringing=.true.
      else
         ringing=.false.
      end if
      _RETURN(_SUCCESS)
   end function is_ringing

end module MAPL_SimpleAlarm
