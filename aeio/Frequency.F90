#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FrequencyMod
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   implicit none
   private

   public :: hinterval

   type :: hinterval
      private
      !character(:), allocatable :: frequency
      integer :: int_frequency
      type(ESMF_Alarm) :: alarm
   contains
      procedure :: initialize
      procedure :: get_frequency
      procedure :: set_alarm
      procedure :: check_if_ringing
   end type hinterval
contains
   subroutine initialize(this, freq, rc)
      class(hinterval), intent(inout) :: this
      character(*),     intent(in   ) :: freq
      integer, optional, intent(out) :: rc
      !integer,     intent(in   ) :: freq
      integer :: status

      read(freq,*,iostat=status)this%int_frequency 
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine initialize

   function get_frequency(this) result(freq)
      !character(:), allocatable :: freq
      integer :: freq
      class(hinterval), intent(inout) :: this

      freq = this%int_frequency
   end function get_frequency

   subroutine set_alarm(this, clock, rc)
      class(hinterval), intent(inout) ::  this
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      type(ESMF_TimeInterval) :: time_interval
      type(ESMF_Time) :: current_time,ring_time
      integer :: hour,minute,second,status

      hour = this%int_frequency/10000
      minute = mod(this%int_frequency/100,100)
      second = mod(this%int_frequency,100)
      call ESMF_TimeIntervalSet(time_interval,h=hour,m=minute,s=second,_RC)
      call ESMF_ClockGet(clock,currTime=current_time,_RC)
      ring_time = current_time+time_interval
      this%alarm = ESMF_AlarmCreate(clock,ringTime=ring_time,ringInterval=time_interval,sticky=.false.,_RC)
      _RETURN(_SUCCESS)

   end subroutine set_alarm

   function check_if_ringing(this,rc) result(ringing)
      class(hinterval), intent(inout) ::  this
      integer, optional, intent(out) :: rc

      logical :: ringing
      integer :: status

      ringing = ESMF_AlarmIsRinging(this%alarm,_RC)
      _RETURN(_SUCCESS)

   end function 

end module FrequencyMod
