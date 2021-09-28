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
      character(:), allocatable :: frequency
      !integer :: frequency
      type(ESMF_Alarm) :: alarm
   contains
      procedure :: initialize
      procedure :: get_frequency
      !procedure :: set_alarm
      !procedure :: check_if_ringing
   end type Frequency
contains
   subroutine initialize(this, freq)
      class(Frequency), intent(inout) :: this
      character(*),     intent(in   ) :: freq
      !integer,     intent(in   ) :: freq

      this%frequency = freq
   end subroutine initialize

   function get_frequency(this) result(freq)
      character(:), allocatable :: freq
      class(Frequency), intent(inout) :: this

      freq = this%frequency
   end function get_frequency

   !subroutine set_alarm(this, clock, rc)
      !class(Frequency), intent(inout) ::  this
      !type(ESMF_Clock), intent(inout) :: clock
      !integer, optional, intent(out) :: rc

      !type(ESMF_TimeInterval) :: time_interval
      !integer :: hour,minute,second

      !hour = this%frequency/10000
      !minute = mod(this%frequency/100,100)
      !second = mod(this%frequency,100)
      !call ESMF_TimeIntervalSet(time_interval,h=hour,m=minute,s=second,__RC__)
      !_RETURN(_SUCCESS)

   !end subroutine set_alarm

   !function check_if_ringing(this,rc) result(ringing)
      !class(Frequency), intent(inout) ::  this
      !integer, optional, intent(out) :: rc

      !logical :: ringing
      !integer :: status

      !ringing = this%ESMF_AlarmIsRinging(this%alarm,__RC__)
      !_RETURN(_SUCCESS)

   !end function 

end module FrequencyMod
