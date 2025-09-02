#include "unused_dummy.H"
#include "MAPL.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   use MAPL_KeywordEnforcerMod
   implicit none (type, external)
   private

   public :: intervals_and_offset_are_compatible
   public :: interval_is_all_zero

   ! This type provides additional logical fields for TimeInterval.
   ! It allows checks on the values of the fields without calling
   ! the ESMF_TimeIntervalGet subroutine and checking the status each time.
   type :: AugmentedInterval
      type(ESMF_TimeInterval), allocatable :: interval
      integer(kind=I4), allocatable :: fields(:)
      logical :: all_zero = .TRUE.
      logical :: only_years_months = .FALSE.
      logical :: valid = .FALSE.
      integer :: status = -1
   end type AugmentedInterval

   interface AugmentedInterval
      module procedure :: construct_augmented_interval
   end interface AugmentedInterval

contains

   type(AugmentedInterval) function construct_augmented_interval(interval) result(a)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer(kind=I4) :: yy, mm, d, s, ns
      integer :: status
      logical :: yymm_zero, ds_zero

      a%interval = interval
      call ESMF_TimeIntervalGet(interval, yy=yy, mm=mm, d=d, s=s, ns=ns, rc=status)
      a%status = status
      yymm_zero = all([yy, mm]==0)
      ds_zero = all([d, s, ns]==0)
      a%all_zero = yymm_zero .and. ds_zero
      a%only_years_months = (.not. yymm_zero) .and. ds_zero
      a%valid = status==ESMF_SUCCESS .and. (yymm_zero .or. ds_zero)

   end function construct_augmented_interval
   
   ! intervals must be comparable. Either:
   ! 1) Both have years and/or months only.
   ! 2) Both have day, second, and/or nanosecond only.
   ! 3) The first interval is all zero.
   ! This is because the ESMF_TimeInterval modulo operation returns results that cannot be used
   ! to compare the intervals that are a mix of (years, months) and (days, seconds, nanoseconds).
   ! In addition, the second interval cannot be all zero.
   ! The same is true of the offset and the second interval.
   subroutine intervals_and_offset_are_compatible(interval1, interval2, compatible, unusable, offset, rc)
      type(ESMF_TimeInterval), intent(in) :: interval1
      type(ESMF_TimeInterval), intent(in) :: interval2
      logical, intent(out) :: compatible
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      integer, optional, intent(inout) :: rc
      integer :: status
      type(AugmentedInterval), allocatable :: a1, a2

      _UNUSED_DUMMY(unusable)
      
      a1 = AugmentedInterval(interval1)
      a2 = AugmentedInterval(interval2)
      compatible = a1%valid .and. a2%valid
      _RETURN_UNLESS(compatible)

      if(present(offset)) then
         call intervals_are_compatible(AugmentedInterval(offset), a2, compatible, _RC)
         _RETURN_UNLESS(compatible)
      end if
      _RETURN_IF(a1%interval == a2%interval)

      call intervals_are_compatible(a1, a2, compatible, _RC)
      _RETURN(_SUCCESS)

   end subroutine intervals_and_offset_are_compatible

   subroutine intervals_are_compatible(aug1, aug2, compatible, rc)
      type(AugmentedInterval), intent(in) :: aug1
      type(AugmentedInterval), intent(in) :: aug2
      logical, intent(out) :: compatible
      integer, optional, intent(out) :: rc
      integer :: status
      type(AugmentedInterval) :: augmod
      character(len=64) :: timeString

      if(aug2%all_zero) then
         call ESMF_TimeIntervalGet(aug2%interval, timeString=timeString, _RC)
      end if
      _ASSERT(.not. aug2%all_zero, 'The second interval is all zero: '// trim(timeString))
      compatible = aug1%valid .and. aug2%valid
      _RETURN_IF(aug1%all_zero .or. aug1%interval == aug2%interval)

      compatible = compatible .and. (aug1%only_years_months .eqv. aug2%only_years_months)
      _RETURN_UNLESS(compatible)

      augmod = AugmentedInterval(mod(aug1%interval, aug2%interval))
      _ASSERT(augmod%valid, 'Unable to perform modulo operation')
      compatible = augmod%all_zero
      _RETURN(_SUCCESS)

   end subroutine intervals_are_compatible
   
   subroutine interval_is_all_zero(interval, all_zero, rc)
      type(ESMF_TimeInterval), intent(in) :: interval
      logical, intent(out) :: all_zero
      integer, optional, intent(out) :: rc
      integer :: status
      type(AugmentedInterval) :: aug

      aug=AugmentedInterval(interval)
      _ASSERT(aug%valid, 'Unable to determine values for time interval')
      all_zero = aug%all_zero
      _RETURN(_SUCCESS)

   end subroutine interval_is_all_zero

end module mapl3g_ESMF_Time_Utilities
