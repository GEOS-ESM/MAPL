#include "MAPL.h"
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

   ! This value should not be accessed directly. Use get_zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The get_zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

   integer, parameter :: SZ_DT = 9

contains

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
!      type(ESMF_TimeInterval) :: mod_value
      integer, optional, intent(inout) :: rc
      integer :: status
      integer, parameter :: MONTH = 2
      integer, parameter :: DAY = MONTH + 1
!      integer(kind=I4) :: f1(SZ_DT), f2(SZ_DT), foff(SZ_DT)
!      character(len=32) :: timeString
!      character(len=:), allocatable :: msg

      associate(ti1 => interval1, ti2 => interval2, off => offset)
         if(present(offset)) then
            !foff = dt_args(off, _RC)
            compatible = intervals_are_compatible(off, ti2, _RC)
            _RETURN_UNLESS(compatible)
            !mod_value = mod(off, ti2)
            !compatible = .not. nonzero(mod_value, _RC)
!            compatible = all(dt_args(mod(off, ti2), rc=status) == 0)
!            _VERIFY(status)
!            call ESMF_TimeIntervalGet(ti2, timeString=timeString, _RC)
!            msg = ': ' // trim(timeString)
!            call ESMF_TimeIntervalGet(offset, timeString=timeString, _RC)
!            msg = msg // ', ' // trim(timeString)
!            _ASSERT(abs_le(ti2, offset), 'Not less than or equal' // msg)
!            _ASSERT(can_compare(f2, foff), "Can't comapre" // msg)
!            _ASSERT(all(dt_args(mod(off, ti2), rc=status) == 0), 'Does not divide' // msg)
         end if
         _RETURN_IF(compatible .and. (ti1 == ti2))
!         f1 = dt_args(ti1, _RC)
         compatible = intervals_are_compatible(ti1, ti2, _RC)
!         compatible = can_compare(f1, f2) .and. all(dt_args(mod(ti1, ti2), rc=status)==0) .and. abs_le(ti2, ti1)
      end associate
      _RETURN(_SUCCESS)

   contains

      function get_fields(interval, rc) result(f)
         integer(kind=I4) :: f(5)
         type(ESMF_TimeInterval), intent(in) :: interval
         integer, optional, intent(out) :: rc
         integer :: status

         call ESMF_TimeIntervalGet(interval, yy=f(1), mm=f(2), d=f(3), s=f(4), ns=f(5), _RC)
         _RETURN(_SUCCESS)

      end function get_fields

      logical function nonzero(interval, rc) result(lval)
         type(ESMF_TimeInterval), intent(in) :: interval
         integer, optional, intent(out) :: rc
         integer :: status
         integer(kind=I4), allocatable, target :: fields(:)
         integer(kind=I4), pointer :: yymm(:) => null()
         integer(kind=I4), pointer :: ds(:) => null()

         fields = get_fields(interval, _RC)
         yymm => fields(:MONTH)
         ds => fields(DAY:)
         lval = (any(yymm /= 0) .and. all(ds == 0)) .or. (all(yymm == 0) .and. any(ds /= 0))
         _RETURN(_SUCCESS)

      end function nonzero

      logical function zero_valued(interval, rc)
         type(ESMF_TimeInterval), intent(in) :: interval
         integer, optional, intent(out) :: rc
         integer :: status
         integer(kind=I4), allocatable :: fields(:)

         fields = get_fields(interval, _RC)
         zero_valued = all(fields == 0)
         _RETURN(_SUCCESS)

      end function zero_valued

      logical function comparable(interval, rc)
         type(ESMF_TimeInterval), intent(in) :: interval
         integer, optional, intent(out) :: rc
         integer :: status
         integer(kind=I4), allocatable, target :: fields(:)
         integer(kind=I4), pointer :: yymm(:) => null()
         integer(kind=I4), pointer :: ds(:) => null()

         fields = get_fields(interval, _RC)
         yymm => fields(:MONTH)
         ds => fields(DAY:)
         comparable = all(ds==0) .neqv. all(yymm==0)
         _RETURN(_SUCCESS)

      end function comparable

      logical function intervals_are_compatible(interval1, interval2, rc) result(lval)
         type(ESMF_TimeInterval), intent(in) :: interval1
         type(ESMF_TimeInterval), intent(in) :: interval2
         integer, optional, intent(out) :: rc
         integer :: status
         integer(kind=I4), allocatable :: fields1(:), fields2(:)
         type(ESMF_TimeInterval) :: abs1, abs2

         lval = comparable(interval1, _RC)
         lval = lval .and. comparable(interval2, _RC)
         _RETURN_UNLESS(lval)

         fields1 = get_fields(interval1, _RC)
         fields2 = get_fields(interval2, _RC)

         lval = all(fields1(DAY:)==0) .eqv. all(fields2(DAY:)==0)
         _RETURN_UNLESS(lval)

         abs1 = ESMF_TimeIntervalAbsValue(interval1)
         abs2 = ESMF_TimeIntervalAbsValue(interval2)
         lval = (abs1 >= abs2) .and. zero_valued(mod(interval1, interval2), _RC)
         _RETURN(_SUCCESS)

      end function intervals_are_compatible

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

      logical function only_yy_mm(f)
         integer(kind=I4), intent(in) :: f(:)
         integer, parameter :: DAY = 3

         only_yy_mm = all(f(DAY:) == 0)

      end function only_yy_mm

      logical function abs_le(interval1, interval2)
         type(ESMF_TimeInterval), intent(in) :: interval1, interval2

!         abs_le = ESMF_TimeIntervalAbsValue(interval1) <= ESMF_TimeIntervalAbsValue(interval2)
         abs_le = interval1 <= interval2

      end function abs_le
         
      logical function can_compare(f1, f2)
         integer(kind=I4), intent(in) :: f1(:), f2(:)
         
         can_compare = only_yy_mm(f1) .and. only_yy_mm(f2) .or. no_yy_mm(f1) .and. no_yy_mm(f2)

      end function can_compare

      logical function no_yy_mm(f)
         integer(kind=I4), intent(in) :: f(:)
         integer, parameter :: DAY = 3
         
         no_yy_mm = all(f(:DAY-1) == 0)

      end function no_yy_mm

      function dt_args(interval, rc) result(a)
         integer(kind=I4) :: a(SZ_DT)
         type(ESMF_TimeInterval), intent(in) :: interval
         integer, optional, intent(out) :: rc
         integer(kind=I4) :: yy, mm, d, h, m, s, ms, us, ns
         integer :: status

         call ESMF_TimeIntervalGet(interval, mm=mm, d=d, h=h, m=m, s=s, ms=ms, us=us, ns=ns, _RC)
         a = [0, mm, d, h, m, s, ms, us, ns]
         _RETURN(_SUCCESS)

      end function dt_args

end module mapl3g_ESMF_Time_Utilities
