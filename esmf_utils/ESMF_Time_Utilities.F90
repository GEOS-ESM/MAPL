#include "MAPL_Generic.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   implicit none (type, external)
   private

   public :: zero_time_interval
   public :: intervals_and_offset_are_compatible

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
      type(ESMF_TimeInterval), intent(in) :: larger
      type(ESMF_TimeInterval), intent(in) :: smaller
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
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      type(ESMF_TimeInterval), intent(in) :: interval1
      type(ESMF_TimeInterval), intent(in) :: interval2
      logical, intent(out) :: compatible
      integer, optional, intent(inout) :: rc
      integer :: status
      type(ESMF_TimeInterval) :: absdiff

      call intervals_are_compatible(interval1, interval2, compatible, _RC)
      _RETURN_UNLESS(compatible)
      absdiff = ESMF_TimeIntervalAbsValue(time1 - time2)
      compatible = mod(absdiff, ESMF_TimeIntervalAbsValue(interval2)) == zero_time_interval()
      _RETURN(_SUCCESS)

   end subroutine times_and_intervals_are_compatible

   ! intervals must be comparable, abs(interval1) >= abs(interval2)
   ! abs(interval2) must evenly divide absolute difference of times
   subroutine intervals_and_offset_are_compatible(interval, interval2, offset, compatible, rc)
      type(ESMF_TimeInterval), intent(in) :: interval
      type(ESMF_TimeInterval), intent(in) :: interval2
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      logical, intent(out) :: compatible
      integer, optional, intent(inout) :: rc
      integer :: status
      type(ESMF_TimeInterval), pointer  :: zero => null()
      integer(kind=I4) :: units(NUM_INTERVAL_UNITS), units2(NUM_INTERVAL_UNITS)

      compatible = .FALSE.
      zero => zero_time_interval()
      _ASSERT(interval2 /= zero, 'The second interval must be nonzero.')
      units = to_array(interval, _RC)
      units2 = to_array(interval2, _RC)
      _RETURN_IF(cannot_compare(units == 0, units2 == 0))
      associate(abs1 => ESMF_TimeIntervalAbsValue(interval), &
            & abs2 => ESMF_TimeIntervalAbsValue(interval2))
         _RETURN_IF(abs1 < abs2 .or. mod(abs1, abs2) /= zero)
         compatible = abs1 >= abs2 .and. mod(abs1, abs2) == zero
         _RETURN_UNLESS(present(offset))
         compatible = compatible .and. mod(ESMF_TimeIntervalAbsValue(offset), abs2) == zero
      end associate
      _RETURN(_SUCCESS)

      contains

!     These combinations (larger, smaller): (yy and/or mm, d), (yy and/or mm, h),
!     (yy and/or mm, m), and (yy and/or mm, s) do not work because the
!     ESMF_TimeInterval overload of the mod function gives incorrect results for
!     these combinations. Presumably ms, us, and ns for the smaller interval do
!     not work.

      logical function cannot_compare(z, z2)
         logical, intent(in) :: z(:), z2(:)
         integer, parameter :: MONTH = 2

         cannot_compare = any(z .neqv. z2) .or. .not. (all(z(:MONTH)) .or. all(z(MONTH+1:)))

      end function cannot_compare

   end subroutine intervals_and_offset_are_compatible

   function get_zero() result(zero)
      type(ESMF_TimeInterval), pointer :: zero
      logical, save :: zero_is_uninitialized = .TRUE.

      if(zero_is_uninitialized) then
         call ESMF_TimeIntervalSet(ZERO_TI, ns=0)
         zero_is_uninitialized = .FALSE.
      end if
      zero => ZERO_TI

   end function get_zero

   function to_array(interval, rc) result(units)
      integer(kind=I4) :: units(NUM_INTERVAL_UNITS)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_TimeIntervalGet(interval, yy=units(1), mm=units(2), d=units(3), &
         & h=units(4), m=units(5), s=units(6), ms=units(7), us=units(8), ns=units(9), _RC)
      _RETURN(_SUCCESS)

   end function to_array

end module mapl3g_ESMF_Time_Utilities
