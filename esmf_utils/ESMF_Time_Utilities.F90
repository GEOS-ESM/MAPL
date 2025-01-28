#include "MAPL_Generic.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   implicit none (type, external)
!   private !wdb fixme deleteme should this be private

   public :: zero_time_interval
   public :: intervals_are_compatible
   public :: times_and_intervals_are_compatible

   interface zero_time_interval
      module procedure :: get_zero
   end interface zero_time_interval

   integer, parameter :: NUM_DATETIME_FIELDS = 6

   type :: DateTimeCheck
   end type DateTimeCheck

   ! This value should not be accessed directly. Use get_zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The get_zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

contains

   logical function can_compare_intervals(larger, smaller, rc)
      type(ESMF_TimeInterval), intent(inout) :: larger
      type(ESMF_TimeInterval), intent(inout) :: smaller
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind=I4) :: lyy, lmm, ld, lh, lm, ls
      integer(kind=I4) :: syy, smm, sd, sh, sm, ss

      can_compare_intervals = .FALSE.
      call ESMF_TimeIntervalGet(larger, yy=lyy, mm=lmm, d=ld, h=lh, m=lm, s=ls, _RC)
      call ESMF_TimeIntervalGet(smaller, yy=syy, mm=smm, d=sd, h=sh, m=sm, s=ss, _RC)
      _RETURN_UNLESS(all([lyy, lmm, syy, smm]==0))
      can_compare_intervals = .TRUE.
      _RETURN(_SUCCESS)

   end function can_compare_intervals

   function get_zero() result(zero)
      type(ESMF_TimeInterval), pointer :: zero
      logical, save :: zero_is_uninitialized = .TRUE.

      if(zero_is_uninitialized) then
         call ESMF_TimeIntervalSet(ZERO_TI, ns=0)
         zero_is_uninitialized = .FALSE.
      end if
      zero => ZERO_TI

   end function get_zero

   logical function intervals_are_compatible(larger, smaller, dst, leap_seconds, rc) result(compatible)
      type(ESMF_TimeInterval), intent(inout) :: larger
      type(ESMF_TimeInterval), intent(inout) :: smaller
      logical, optional, intent(in) :: dst
      logical, optional, intent(in) :: leap_seconds
      integer, optional, intent(out) :: rc
      integer :: status

      associate(abs_larger => ESMF_TimeIntervalAbsValue(larger), abs_smaller => ESMF_TimeIntervalAbsValue(smaller))
         compatible = abs_larger >= abs_smaller
         _RETURN_UNLESS(compatible)
         compatible = can_compare_intervals(larger, smaller, _RC)
         _RETURN_UNLESS(compatible)
         compatible = mod(abs_larger, abs_smaller) == get_zero()
      end associate

      _RETURN(_SUCCESS)

   end function intervals_are_compatible

   logical function times_and_intervals_are_compatible(time1, time2, larger, smaller, rc) result(compatible)
      type(ESMF_Time), intent(inout) :: time1
      type(ESMF_Time), intent(inout) :: time2
      type(ESMF_TimeInterval), target, intent(inout) :: larger
      type(ESMF_TimeInterval), target, intent(inout) :: smaller
      integer, optional, intent(inout) :: rc
      integer :: status
      type(ESMF_TimeInterval), pointer :: interval => null()

      compatible = intervals_are_compatible(larger, smaller, _RC)
      _RETURN_UNLESS(compatible)
      compatible = mod(ESMF_TimeIntervalAbsValue(time1 - time2), ESMF_TimeIntervalAbsValue(smaller)) == get_zero()
      _RETURN(_SUCCESS)

   end function times_and_intervals_are_compatible

end module mapl3g_ESMF_Time_Utilities
