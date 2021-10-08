#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FrequencyMod
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   implicit none
   private

   public :: Frequency

   type :: Frequency
      private
      !character(:), allocatable :: frequency
      integer :: frequency
      type(ESMF_Alarm) :: alarm
   contains
      procedure :: initialize
      procedure :: get_frequency
      procedure :: set_alarm
      procedure :: check_if_ringing
   end type Frequency
contains
   subroutine initialize(this, freq, rc)
      class(Frequency), intent(inout) :: this
      character(*),     intent(in   ) :: freq
      integer, optional, intent(out) :: rc
      !integer,     intent(in   ) :: freq
      integer :: status

      !this%frequency = freq
      read(freq,*,iostat=status)this%frequency 
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine initialize

   function get_frequency(this) result(freq)
      !character(:), allocatable :: freq
      integer :: freq
      class(Frequency), intent(inout) :: this

      freq = this%frequency
   end function get_frequency

   subroutine set_alarm(this, clock, rc)
      class(Frequency), intent(inout) ::  this
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      type(ESMF_TimeInterval) :: time_interval
      type(ESMF_Time) :: current_time,ring_time
      integer :: hour,minute,second,status

      hour = this%frequency/10000
      minute = mod(this%frequency/100,100)
      second = mod(this%frequency,100)
      call ESMF_TimeIntervalSet(time_interval,h=hour,m=minute,s=second,_RC)
      call ESMF_ClockGet(clock,currTime=current_time,_RC)
      ring_time = current_time+time_interval
      this%alarm = ESMF_AlarmCreate(clock,ringTime=ring_time,ringInterval=time_interval,sticky=.false.,_RC)
      _RETURN(_SUCCESS)

   end subroutine set_alarm

   function check_if_ringing(this,rc) result(ringing)
      class(Frequency), intent(inout) ::  this
      integer, optional, intent(out) :: rc

      logical :: ringing
      integer :: status

      ringing = ESMF_AlarmIsRinging(this%alarm,_RC)
      _RETURN(_SUCCESS)

   end function 

end module FrequencyMod
