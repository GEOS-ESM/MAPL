#include "unused_dummy.H"
#include "MAPL.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   use MAPL_KeywordEnforcerMod
   implicit none (type, external)
   private

   public :: zero_time_interval
   public :: intervals_and_offset_are_compatible

   type :: AugmentedInterval
      type(ESMF_TimeInterval), allocatable :: interval
      integer(kind=I4), allocatable :: fields(:)
      character(len=:), allocatable :: string
      logical :: all_zero = .TRUE.
      logical :: yy_mm_only = .FALSE.
      logical :: d_s_only = .FALSE.
      logical :: valid = .FALSE.
      integer :: status = -1
   contains
      procedure :: get_yy_mm
      procedure :: get_d_s
   end type AugmentedInterval

   interface AugmentedInterval
      module procedure :: construct_augmented_interval
   end interface AugmentedInterval

   interface zero_time_interval
      module procedure :: get_zero
   end interface zero_time_interval

   ! This value should not be accessed directly. Use get_zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The get_zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

   integer, parameter :: MONTH = 2
   integer, parameter :: DAY = MONTH + 1

contains

   function construct_augmented_interval(interval) result(augint)
      type(AugmentedInterval) :: augint
      type(ESMF_TimeInterval), intent(in) :: interval
      integer(kind=I4), allocatable :: fields(:)
      integer :: status
      character(len=32) :: string
      logical :: valid

      call ESMF_TimeIntervalGet(interval, timeString=string)
      augint%string = trim(string)
      fields = get_fields(interval, rc=status)
      augint%status = status
      valid = (status==ESMF_SUCCESS)
      augint%valid = valid
      if(.not. valid) then
         augint%string = augint%string // ' (could not get fields)'
         return
      end if
      augint%interval = interval
      augint%fields = fields
      augint%valid = .TRUE.
      augint%all_zero = all(fields==0)
      if(augint%all_zero) return
      augint%valid = all(augint%get_yy_mm()==0) .or. all(augint%get_d_s()==0)
      if(.not. augint%valid) return
      augint%yy_mm_only = all(augint%get_d_s()==0)
      augint%d_s_only = all(augint%get_yy_mm()==0)

   end function construct_augmented_interval
   
   function get_yy_mm(this) result(yymm)
      integer(kind=I4), allocatable :: yymm(:)
      class(AugmentedInterval), intent(in) :: this

      yymm = this%fields(:MONTH)

   end function get_yy_mm

   function get_d_s(this) result(d_s)
      integer(kind=I4), allocatable :: d_s(:)
      class(AugmentedInterval), intent(in) :: this

      d_s = this%fields(DAY:)

   end function get_d_s

   ! intervals must be comparable: both yy and/or mm only, no yy or mm, or the first all zero.
   ! These combinations (larger, smaller): (yy and/or mm, d), (yy and/or mm, h),
   ! (yy and/or mm, m), and (yy and/or mm, s) do not work because the
   ! ESMF_TimeInterval overload of the mod function gives incorrect results for
   ! these combinations. Presumably ms, us, and ns for the smaller interval do
   ! not work. The same is true of the offset and the second interval.
   subroutine intervals_and_offset_are_compatible(interval1, interval2, compatible, unusable, offset, rc)
      type(ESMF_TimeInterval), intent(in) :: interval1
      type(ESMF_TimeInterval), intent(in) :: interval2
      logical, intent(out) :: compatible
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      integer, optional, intent(inout) :: rc
      integer :: status
      type(AugmentedInterval), allocatable :: aug1, aug2, aug0

      _UNUSED_DUMMY(unusable)
      aug1 = AugmentedInterval(interval1)
      compatible = aug1%valid
      _RETURN_UNLESS(compatible)

      aug2 = AugmentedInterval(interval2)
      compatible = aug2%valid
      _RETURN_UNLESS(compatible)

      if(present(offset)) then
         aug0 = AugmentedInterval(offset)
         compatible = aug0%valid
      end if
      _RETURN_UNLESS(compatible)

      _ASSERT(.not. aug2%all_zero, 'The second interval cannot be 0.')

      if(present(offset)) then
         call intervals_are_compatible(aug0, aug2, compatible, _RC)
      end if
      _RETURN_UNLESS(compatible)

      _RETURN_IF(compatible .and. (aug1%interval == aug2%interval))

      call intervals_are_compatible(aug1, aug2, compatible, _RC)
      _RETURN(_SUCCESS)

   end subroutine intervals_and_offset_are_compatible

   function get_fields(interval, rc) result(f)
      integer(kind=I4) :: f(5)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_TimeIntervalGet(interval, yy=f(1), mm=f(2), d=f(3), s=f(4), ns=f(5), _RC)
      _RETURN(_SUCCESS)

   end function get_fields

   logical function all_zero(interval, rc)
      type(ESMF_TimeInterval), intent(in) :: interval
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind=I4), allocatable :: fields(:)

      fields = get_fields(interval, _RC)
      all_zero = all(fields == 0)
      _RETURN(_SUCCESS)

   end function all_zero

   subroutine is_valid(interval, comparable, rc) 
      type(ESMF_TimeInterval), intent(in) :: interval
      logical, intent(out) :: comparable
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind=I4), allocatable :: fields(:)

      fields = get_fields(interval, _RC)
      comparable = all(fields == 0) .or. (all(fields(DAY:)==0) .neqv. all(fields(:MONTH)==0))
      _RETURN(_SUCCESS)

   end subroutine is_valid

   subroutine intervals_are_compatible(aug1, aug2, compatible, rc)
      type(AugmentedInterval), intent(in) :: aug1
      type(AugmentedInterval), intent(in) :: aug2
      logical, intent(out) :: compatible
      integer, optional, intent(out) :: rc
      integer :: status
      type(AugmentedInterval) :: augmod

      _ASSERT(.not. aug2%all_zero, 'The second interval is all zero.')
      compatible = aug1%all_zero .or. aug1%interval == aug2%interval
      _RETURN_IF(compatible)

      compatible = (aug1%yy_mm_only .and. aug2%yy_mm_only) .or. (aug1%d_s_only .and. aug2%d_s_only)
      _RETURN_UNLESS(compatible)

      augmod = AugmentedInterval(mod(aug1%interval, aug2%interval))
      _ASSERT(augmod%valid, 'Unable to perform modulo operation')
      compatible = augmod%all_zero
      _RETURN(_SUCCESS)

   end subroutine intervals_are_compatible
   
   ! MAY DELETE wdb
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
