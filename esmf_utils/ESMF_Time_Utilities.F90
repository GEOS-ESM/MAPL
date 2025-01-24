!wdb fixme deleteme should this be different include file
#include "MAPL_Generic.h"
module mapl3g_ESMF_Time_Utilities
   use esmf
   use mapl_ErrorHandling
   implicit none !wdb fixme deleteme hsould replace this with new implicit none
!   private !wdb fixme deleteme should this be private

   public :: zero_time_interval
   public :: intervals_are_compatible
   public :: times_and_intervals_are_compatible
   public :: interval_is_monthly
   public :: interval_is_yearly
   public :: interval_is_uniform
   public :: UNIFORM
   public :: MONTHLY
   public :: YEARLY

   interface zero_time_interval
      module procedure :: get_zero
   end interface zero_time_interval

   integer, parameter :: UNIFORM = 1
   integer, parameter :: MONTHLY = 2
   integer, parameter :: YEARLY =  3

   ! This value should not be accessed directly. Use get_zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The get_zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

contains

   function get_zero() result(zero)
      type(ESMF_TimeInterval), pointer :: zero
      logical, save :: zero_is_uninitialized = .TRUE.

      if(zero_is_uninitialized) then
         call ESMF_TimeIntervalSet(ZERO_TI, ns=0)
         zero_is_uninitialized = .FALSE.
      end if
      zero => ZERO_TI

   end function get_zero

   logical function intervals_are_compatible(larger, smaller, rc) result(compatible)
      type(ESMF_TimeInterval), intent(in) :: larger
      type(ESMF_TimeInterval), intent(in) :: smaller
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: interval_type, interval_type2

      associate(abs_larger => ESMF_TimeIntervalAbsValue(larger), abs_smaller => ESMF_TimeIntervalAbsValue(smaller))
         if(abs_larger < abs_smaller) then
            compatible = intervals_are_compatible(smaller, larger, _RC)
            _RETURN(_SUCCESS)
         end if

         compatible = .FALSE.
         interval_type = get_interval_type(larger)
         interval_type2 = get_interval_type(smaller)
         compatible = interval_type == interval_type2
         _RETURN_UNLESS(compatible)
         compatible = mod(abs_larger, abs_smaller) == get_zero()
      end associate

      _RETURN(_SUCCESS)

   end function intervals_are_compatible

   logical function times_and_intervals_are_compatible(time1, time2, larger, smaller, rc) result(compatible)
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      type(ESMF_TimeInterval), target, intent(in) :: larger
      type(ESMF_TimeInterval), target, intent(in) :: smaller
      integer, optional, intent(in) :: rc
      integer :: status
      logical :: compatible
      type(ESMF_TimeInterval), pointer :: interval => null()

      compatible = ESMF_TimeIntervalAbsValue(larger) >= ESMF_TimeIntervalAbsValue(smaller)
      _RETURN_UNLESS(compatible)
      compatible = intervals_are_compatible(larger, smaller, _RC)
      _RETURN_UNLESS(compatible)
      compatible = mod(ESMF_TimeIntervalAbsValue(time1 - time2), ESMF_TimeInterval(smaller))
      _RETURN(_SUCCESS)

   end function times_and_intervals_are_compatible

   logical function interval_is_monthly(interval, rc)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer, optional, intent(out) :: rc 
      integer :: status
      integer :: mm
      logical :: yearly

      yearly = interval_is_yearly(interval, _RC)
      call ESMF_TimeIntervalGet(interval, mm=mm, _RC)
      interval_is_monthly = .not. yearly .and. mm /= 0
      _RETURN(_SUCCESS)

   end function interval_is_monthly(interval, rc)

   logical function interval_is_yearly(interval, rc)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer, optional, intent(out) :: rc 
      integer :: status
      integer :: yy

      call ESMF_TimeIntervalGet(interval, yy=yy, _RC)
      interval_is_yearly = yy /= 0
      _RETURN(_SUCCESS)

   end function interval_is_yearly(interval, rc)

   logical function interval_is_uniform(interval, rc) result(is_uniform)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer, optional, intent(out) :: rc 
      integer :: status
      logical :: is_yearly
      logical :: is_monthly
      
      is_uniform = .FALSE.
      is_yearly = interval_is_yearly(interval, _RC)
      _RETURN_IF(is_yearly)
      is_monthly = interval_is_monthly(interval, _RC)
      _RETURN_IF(is_monthly)
      is_uniform = .TRUE.
      _RETURN(_SUCCESS)

   end function interval_is_uniform

   function get_interval_type(interval, rc) result(interval_type)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer, optional, intent(out) :: rc
      integer :: status
      integer, intent(out) :: interval_type
      logical :: lval

      interval_type = MONTHLY
      lval = interval_is_monthly(interval, _RC)
      if(lval) return

      interval_type = YEARLY
      lval = interval_is_yearly(interval, _RC)
      if(lval) return

      interval_type = UNIFORM

   end function get_interval_type

end module mapl3g_ESMF_Time_Utilities
