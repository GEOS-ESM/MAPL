#include "MAPL_Generic.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   implicit none (type, external)
   private

   public :: zero_time_interval
   public :: intervals_are_compatible
   public :: times_and_intervals_are_compatible

   interface zero_time_interval
      module procedure :: get_zero
   end interface zero_time_interval

   integer, parameter :: NUM_INTERVAL_UNITS = 9

   ! This value should not be accessed directly. Use get_zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The get_zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

contains

   ! must be possible to compare intervals, based on their nonzero units
   ! smaller interval must divide the larger interval evenly
   ! assumes they have the same sign.
   subroutine intervals_are_compatible(larger, smaller, compatible, rc)
      type(ESMF_TimeInterval), intent(inout) :: larger
      type(ESMF_TimeInterval), intent(inout) :: smaller
      logical, intent(out) :: compatible
      integer, optional, intent(out) :: rc
      integer :: status

      _ASSERT(smaller /= get_zero(), 'The smaller unit must be nonzero.')
      associate(abs_larger => ESMF_TimeIntervalAbsValue(larger), abs_smaller => ESMF_TimeIntervalAbsValue(smaller))
         compatible = abs_larger >= abs_smaller
         _RETURN_UNLESS(compatible)
         call can_compare_intervals(larger, smaller, compatible, _RC)
         _RETURN_UNLESS(compatible)
         compatible = mod(abs_larger, abs_smaller) == get_zero()
      end associate

      _RETURN(_SUCCESS)

   end subroutine intervals_are_compatible

   ! intervals must be comparable, abs(interval1) >= abs(interval2)
   ! abs(interval2) must evenly divide absolute difference of times
   subroutine times_and_intervals_are_compatible(interval1, time1, interval2, time2, compatible, rc)
      type(ESMF_Time), intent(inout) :: time1
      type(ESMF_Time), intent(inout) :: time2
      type(ESMF_TimeInterval), intent(inout) :: interval1
      type(ESMF_TimeInterval), intent(inout) :: interval2
      logical, intent(out) :: compatible
      integer, optional, intent(inout) :: rc
      integer :: status
      type(ESMF_TimeInterval) :: absdiff

      call intervals_are_compatible(interval1, interval2, compatible, _RC)
      _RETURN_UNLESS(compatible)
      absdiff = ESMF_TimeIntervalAbsValue(time1 - time2)
      compatible = mod(absdiff, ESMF_TimeIntervalAbsValue(interval2)) == get_zero()
      _RETURN(_SUCCESS)

   end subroutine times_and_intervals_are_compatible

!   These combinations (larger, smaller): (yy and/or mm, d), (yy and/or mm, h),
!   (yy and/or mm, m), and (yy and/or mm, s) do not work because the
!   ESMF_TimeInterval overload of the mod function gives incorrect results for
!   these combinations. Presumably ms, us, and ns for the smaller interval do
!   not work.
   subroutine can_compare_intervals(larger, smaller, comparable, rc)
      type(ESMF_TimeInterval), intent(inout) :: larger
      type(ESMF_TimeInterval), intent(inout) :: smaller
      logical, intent(out) :: comparable
      integer, optional, intent(out) :: rc
      integer :: status

      comparable = has_only_years_and_months(larger, _RC)
      comparable = comparable .and. has_only_years_and_months(smaller, _RC)
      _RETURN_IF(comparable)

      comparable = has_no_years_or_months(larger, _RC)
      comparable = comparable .and. has_no_years_or_months(smaller, _RC)
      _RETURN(_SUCCESS)

   end subroutine can_compare_intervals
      
   function get_zero() result(zero)
      type(ESMF_TimeInterval), pointer :: zero
      logical, save :: zero_is_uninitialized = .TRUE.

      if(zero_is_uninitialized) then
         call ESMF_TimeIntervalSet(ZERO_TI, ns=0)
         zero_is_uninitialized = .FALSE.
      end if
      zero => ZERO_TI

   end function get_zero

   subroutine as_array(interval, units, rc)
      type(ESMF_TimeInterval), intent(inout) :: interval
      integer(kind=I4), intent(out) :: units(NUM_INTERVAL_UNITS)
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_TimeIntervalGet(interval, yy=units(1), mm=units(2), d=units(3), &
         & h=units(4), m=units(5), s=units(6), ms=units(7), us=units(8), ns=units(9), _RC)
      _RETURN(_SUCCESS)

   end subroutine as_array

   logical function has_only_years_and_months(interval, rc)
      type(ESMF_TimeInterval), intent(inout) :: interval
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind=I4) :: units(NUM_INTERVAL_UNITS)

      call as_array(interval, units, _RC)
      has_only_years_and_months = all(units(3:) == 0)
      _RETURN(_SUCCESS)

   end function has_only_years_and_months

   logical function has_no_years_or_months(interval, rc)
      type(ESMF_TimeInterval), intent(inout) :: interval
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind=I4) :: units(NUM_INTERVAL_UNITS)

      call as_array(interval, units, _RC)
      has_no_years_or_months = all(units(1:2) == 0)
      _RETURN(_SUCCESS)

   end function has_no_years_or_months

end module mapl3g_ESMF_Time_Utilities
