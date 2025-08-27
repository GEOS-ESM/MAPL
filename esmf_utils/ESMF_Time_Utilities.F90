#include "MAPL.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   implicit none (type, external)
   private

   public :: zero_time_interval
   public :: intervals_and_offset_are_compatible
   public :: comparable

   type :: TimeIntervalInfo
      logical :: comparable = .FALSE.
      logical :: only_yy_mm = .FALSE.
      logical :: no_yy_mm = .FALSE.
      logical :: one_day = .FALSE.
      logical :: nonzero = .FALSE.
   end type TimeIntervalInfo

   interface zero_time_interval
      module procedure :: get_zero
   end interface zero_time_interval

   ! This value should not be accessed directly. Use get_zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The get_zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

   integer, parameter :: NUM_DT_PARTS = 9

contains

   subroutine interval_to_array(interval, a, rc)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer(kind=I4), intent(out) :: a(NUM_DT_PARTS)
      integer, optional, intent(out) :: rc
      integer(kind=I4) :: yy, mm, d, h, m, s, ms, us, ns
      integer :: status

      call ESMF_TimeIntervalGet(interval, yy=yy, mm=mm, d=d, h=h, m=m, s=s, ms=ms, us=us, ns=ns, _RC)
      a = [yy, mm, d, h, m, s, ms, us, ns]
      _RETURN(_SUCCESS)

   end subroutine interval_to_array

   type(TimeIntervalInfo) function get_time_interval_info(interval) result(info)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer(kind=I4) :: val(NUM_DT_PARTS)
      integer, parameter :: DAY = 3
      integer :: rc
      logical :: only_yy_mm, no_yy_mm, one_day

      call interval_to_array(interval, val, rc=rc)
      info%comparable = (rc == ESMF_SUCCESS)
      if(not(info%comparable)) return
      info%nonzero = any(val /= 0_I4)
      info%only_yy_mm = all(val(DAY:) == 0)
      info%no_yy_mm = all(val(:DAY-1) == 0)
      info%one_day = info%no_yy_mm .and. val(DAY) == 1 .and. all(val(DAY+1:) == 0)

   end function get_time_interval_info

   logical function comparable(larger, smaller) result(lval)
      type(ESMF_TimeInterval), intent(in) :: larger, smaller
      type(TimeIntervalInfo) :: linfo, sminfo

      lval = larger == smaller
      if(lval) return
      linfo = get_time_interval_info(larger)
      sminfo = get_time_interval_info(smaller)
      if(not(linfo%comparable .and. sminfo%comparable .and. sminfo%nonzero)) return
      if(linfo%no_yy_mm) then
         lval = sminfo%no_yy_mm
         return
      end if
      lval = linfo%only_yy_mm .and. (sminfo%only_yy_mm .or. sminfo%one_day)

   end function comparable

   ! intervals must be comparable, abs(interval1) >= abs(interval2)
   ! abs(interval2) must evenly divide absolute difference of times
   ! These combinations (larger, smaller): (yy and/or mm, d), (yy and/or mm, h),
   ! (yy and/or mm, m), and (yy and/or mm, s) do not work because the
   ! ESMF_TimeInterval overload of the mod function gives incorrect results for
   ! these combinations. Presumably ms, us, and ns for the smaller interval do
   ! not work.
   subroutine intervals_and_offset_are_compatible(interval1, interval2, compatible, offset, rc)
      type(ESMF_TimeInterval), intent(in) :: interval1
      type(ESMF_TimeInterval), intent(in) :: interval2
      logical, intent(out) :: compatible
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      integer, optional, intent(inout) :: rc
      integer :: status
      type(ESMF_TimeInterval), pointer  :: zero => null()
      integer, parameter :: DAY = 3
      integer(kind=I4) :: parts1(NUM_DT_PARTS), parts2(NUM_DT_PARTS), partsoff(NUM_DT_PARTS)

      zero => zero_time_interval()
      call interval_to_array(interval2, parts2, _RC)
      if(present(offset)) then
         compatible = .FALSE.
         call interval_to_array(offset, partsoff, _RC)
         _RETURN_UNLESS(can_compare(parts2, partsoff))
         _RETURN_IF(ESMF_TimeIntervalAbsValue(offset) < ESMF_TimeIntervalAbsValue(interval2))
         _RETURN_UNLESS(mod(offset, interval2) == zero)
      end if
      compatible = .TRUE.
      _RETURN_IF(interval1 == interval2)
      compatible = .FALSE.
      _RETURN_UNLESS(interval2 /= zero)
      call interval_to_array(interval1, parts1, _RC)
      _RETURN_UNLESS(can_compare(parts1, parts2))
      _RETURN_IF(ESMF_TimeIntervalAbsValue(interval1) < ESMF_TimeIntervalAbsValue(interval2))
      _RETURN_UNLESS(mod(interval1, interval2) == zero)
      compatible = .TRUE.
      _RETURN(_SUCCESS)

   contains

      logical function can_compare(vals1, vals2)
         integer(kind=I4), intent(in) :: vals1(:), vals2(:)
         
         can_compare = only_yy_mm(vals1) .and. only_yy_mm(vals2) .or. no_yy_mm(vals1) .and. no_yy_mm(vals2)
      end function can_compare

      logical function has_yy_mm(vals)
         integer(kind=I4), intent(in) :: vals(:)

         has_yy_mm = any(vals(:DAY-1) /= 0)

      end function has_yy_mm

      logical function no_yy_mm(vals)
         integer(kind=I4), intent(in) :: vals(:)
         
         no_yy_mm = all(vals(:DAY-1) == 0)

      end function no_yy_mm

      logical function has_time(vals)
         integer(kind=I4), intent(in) :: vals(:)

         has_time = any(vals(DAY+1:) /= 0)

      end function has_time

      logical function has_day(vals)
         integer(kind=I4), intent(in) :: vals(:)

         has_day = vals(DAY) /= 0

      end function has_day

      logical function only_yy_mm(vals)
         integer(kind=I4), intent(in) :: vals(:)

         only_yy_mm = all(vals(DAY:) == 0)
!         only_yy_mm = .not. (has_day(vals) .or. has_time(vals))

      end function only_yy_mm

      logical function one_day(vals)
         integer(kind=I4), intent(in) :: vals(:)

         one_day = .not. has_yy_mm(vals) .and. vals(DAY) == 1 .and. all(vals(DAY+1:) == 0)

      end function one_day

      logical function offset_compatible(interval, offset) result(lval)
         type(ESMF_TimeInterval), intent(in) :: interval
         type(ESMF_TimeInterval), optional, intent(in) :: offset

         lval = .TRUE.
         if(present(offset)) lval = mod(offset, interval) == zero

      end function offset_compatible

      logical function abs_less_equal(vals, vals2) result(lval)
         integer(kind=I4), intent(in) :: vals(:), vals2(:)
         integer(kind=I4), allocatable :: absvals(:), absvals2(:)
         integer :: i

         lval = .TRUE.
         absvals = abs(vals)
         absvals2 = abs(vals2)
         do i=1, size(vals)
            if(absvals(i) < absvals2(i)) return
            if(absvals(i) > absvals2(i)) then
               lval = .FALSE.
               return
            end if
         end do

      end function abs_less_equal

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

end module mapl3g_ESMF_Time_Utilities
