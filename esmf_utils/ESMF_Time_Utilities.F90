#include "unused_dummy.H"
#include "MAPL.h"

module mapl3g_ESMF_Time_Utilities

   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   use MAPL_KeywordEnforcerMod

   implicit none (type, external)
   private

   public :: check_compatibility
   public :: interval_is_all_zero
   public :: sub_time_in_datetime

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

   ! The intervals and offset are compatible if the second interval evenly divides the first interval and
   ! the offset (if present). To check this, intervals must be comparable. The second interval cannot be
   ! all zero. Either, the first interval is all zero, both have years and/or months only, or both have
   ! day, second, and/or nanosecond only. This is because the ESMF_TimeInterval mod operation returns
   ! results that cannot be used to compare the intervals that are a mix of (years, months) & (days,
   ! seconds, nanoseconds). The same is true of the offset and the second interval.
   subroutine check_compatibility(interval1, interval2, compatible, unusable, offset, rc)
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
   end subroutine check_compatibility

   subroutine intervals_are_compatible(aug1, aug2, compatible, rc)
      type(AugmentedInterval), intent(in) :: aug1
      type(AugmentedInterval), intent(in) :: aug2
      logical, intent(out) :: compatible
      integer, optional, intent(out) :: rc
      integer :: status
      type(AugmentedInterval) :: augmod
      character(len=64) :: timeString

      compatible = .FALSE.
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

      type(AugmentedInterval) :: aug

      aug=AugmentedInterval(interval)
      _ASSERT(aug%valid, 'Unable to determine values for time interval')
      all_zero = aug%all_zero

      _RETURN(_SUCCESS)
   end subroutine interval_is_all_zero

   ! Parse a ref_datetime template and substitute time components from the given ESMF_Time.
   ! Template format: YYYY-MM-DDTHH:NN:SS (always 19 characters, fixed positions).
   ! Placeholder tokens (YYYY, MM, DD, HH, NN, SS) mean "use the value from the input time".
   ! Literal numeric values override the corresponding component.
   function sub_time_in_datetime(time, ref_datetime, rc) result(new_time)
      type(ESMF_Time) :: new_time
      type(ESMF_Time), intent(in) :: time
      character(len=*), intent(in) :: ref_datetime
      integer, optional, intent(out) :: rc

      integer :: status, year, month, day, hour, minute, second

      _ASSERT(len(ref_datetime) == 19, 'ref_datetime must be 19 characters: YYYY-MM-DDTHH:NN:SS, got: '//ref_datetime)

      call ESMF_TimeGet(time, yy=year, mm=month, dd=day, h=hour, m=minute, s=second, _RC)

      ! Year (positions 1-4): YYYY = use current year, else read 4-digit integer
      if (ref_datetime(1:4) == 'YYYY') then
         ! year stays as current time year
      else
         read(ref_datetime(1:4), '(I4)') year
      end if

      ! Separator (position 5): must be '-'
      _ASSERT(ref_datetime(5:5) == '-', 'Expected dash separator at position 5 in ref_datetime')

      ! Month (positions 6-7): MM = use current month, else read 2-digit integer
      if (ref_datetime(6:7) == 'MM') then
         ! month stays as current time month
      else
         read(ref_datetime(6:7), '(I2)') month
         _ASSERT(month >= 1 .and. month <= 12, 'ref_datetime month must be between 1 and 12')
      end if

      ! Separator (position 8): must be '-'
      _ASSERT(ref_datetime(8:8) == '-', 'Expected dash separator at position 8 in ref_datetime')

      ! Day (positions 9-10): DD = use current day, else read 2-digit integer
      if (ref_datetime(9:10) == 'DD') then
         ! day stays as current time day
      else
         read(ref_datetime(9:10), '(I2)') day
         _ASSERT(day >= 1 .and. day <= 28, 'ref_datetime day must be between 1 and 28')
      end if

      ! Separator (position 11): must be 'T'
      _ASSERT(ref_datetime(11:11) == 'T', 'Expected T separator at position 11 in ref_datetime')

      ! Hour (positions 12-13): HH = use current hour, else read 2-digit integer
      if (ref_datetime(12:13) == 'HH') then
         ! hour stays as current time hour
      else
         read(ref_datetime(12:13), '(I2)') hour
         _ASSERT(hour >= 0 .and. hour <= 23, 'ref_datetime hour must be between 0 and 23')
      end if

      ! Separator (position 14): must be ':'
      _ASSERT(ref_datetime(14:14) == ':', 'Expected colon separator at position 14 in ref_datetime')

      ! Minute (positions 15-16): NN = use current minute, else read 2-digit integer
      if (ref_datetime(15:16) == 'NN') then
         ! minute stays as current time minute
      else
         read(ref_datetime(15:16), '(I2)') minute
         _ASSERT(minute >= 0 .and. minute <= 59, 'ref_datetime minute must be between 0 and 59')
      end if

      ! Separator (position 17): must be ':'
      _ASSERT(ref_datetime(17:17) == ':', 'Expected colon separator at position 17 in ref_datetime')

      ! Second (positions 18-19): SS = use current second, else read 2-digit integer
      if (ref_datetime(18:19) == 'SS') then
         ! second stays as current time second
      else
         read(ref_datetime(18:19), '(I2)') second
         _ASSERT(second >= 0 .and. second <= 59, 'ref_datetime second must be between 0 and 59')
      end if

      call ESMF_TimeSet(new_time, yy=year, mm=month, dd=day, h=hour, m=minute, s=second, _RC)

      _RETURN(_SUCCESS)
   end function sub_time_in_datetime

end module mapl3g_ESMF_Time_Utilities
