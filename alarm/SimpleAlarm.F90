#include "MAPL.h"

! a really dumb alarm class
! an alarm has an initial ringtime
! and a frequency which defines an infinite number of times
! given an input time, you are either one of those or you aren't 

module mapl3g_SimpleAlarm
   use esmf
   use mapl_ErrorHandling
   implicit none
   private

   public :: SimpleAlarm

   type SimpleAlarm
      type(ESMF_Time) :: initial_ring_time
      type(ESMF_TimeInterval) :: ring_interval
      logical :: use_naive =.false.
      contains
         procedure is_ringing
   end type
         
   interface SimpleAlarm
      module procedure :: construct_simple_alarm
   end interface

   contains

   function construct_simple_alarm(initial_ring_time, ring_interval, rc) result(new_simple_alarm)
      type(SimpleAlarm) :: new_simple_alarm
      type(ESMF_Time), intent(in) :: initial_ring_time
      type(ESMF_TimeInterval), intent(in) :: ring_interval
      integer, optional, intent(out) :: rc

      integer(kind=ESMF_KIND_I4) :: yy, mm, d, s, ns
      logical :: yymm_zero, ds_zero, is_all_zero
      integer :: status
      
      call ESMF_TimeIntervalGet(ring_interval, yy=yy, mm=mm, d=d, s=s, ns=ns, rc=status)
      yymm_zero = all([yy, mm]==0)
      ds_zero = all([d, s, ns]==0)
      is_all_zero = yymm_zero .and. ds_zero

      _ASSERT(.not.is_all_zero, 'ring interval for simple alarm is 0') 

      new_simple_alarm%initial_ring_time = initial_ring_time
      new_simple_alarm%ring_interval = ring_interval
      new_simple_alarm%use_naive = .not. yymm_zero

      _RETURN(_SUCCESS)
   end function

   function is_ringing(this, time, rc) result(ring_state)
      logical :: ring_state
      class(SimpleAlarm), intent(in) :: this
      type(ESMF_Time), intent(in) :: time
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TimeInterval) :: delta, remainder
      type(ESMF_Time) :: temp_time
      logical :: is_all_zero

      ring_state=.false.
      if (this%use_naive) then
         temp_time = this%initial_ring_time
         if (time >= temp_time) then
            do while(temp_time <= time)
               if (temp_time == time) ring_state=.true.
               temp_time=temp_time+this%ring_interval 
            enddo 
         end if
      else
         delta = time-this%initial_ring_time
         remainder = mod(delta,this%ring_interval)
         is_all_zero = all_zero(remainder, _RC)
         if (is_all_zero) ring_state=.true.
      end if
      
   end function 

   function all_zero(time_interval, rc) result(is_all_zero)
      logical :: is_all_zero
      type(ESMF_TimeInterval), intent(in) :: time_interval
      integer, optional, intent(out) :: rc

      integer(kind=ESMF_KIND_I4) :: yy, mm, d, s, ns
      integer :: status
      logical :: yymm_zero, ds_zero
      
      call ESMF_TimeIntervalGet(time_interval, yy=yy, mm=mm, d=d, s=s, ns=ns, rc=status)
      yymm_zero = all([yy, mm]==0)
      ds_zero = all([d, s, ns]==0)
      is_all_zero = yymm_zero .and. ds_zero

      _RETURN(_SUCCESS)
   end function

end module mapl3g_SimpleAlarm
