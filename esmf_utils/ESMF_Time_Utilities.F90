#include "MAPL.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
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
   subroutine intervals_and_offset_are_compatible(interval1, interval2, compatible, offset, message, rc)
      type(ESMF_TimeInterval), intent(in) :: interval1
      type(ESMF_TimeInterval), intent(in) :: interval2
      logical, intent(out) :: compatible
      type(ESMF_TimeInterval), optional, intent(in) :: offset
      character(len=:), optional, allocatable, intent(out) :: message
      integer, optional, intent(inout) :: rc
      integer :: status
      integer :: sz
      type(AugmentedInterval), allocatable :: aug1, aug2, aug0
      character(len=:), allocatable :: message_

      aug1 = AugmentedInterval(interval1)
      compatible = aug1%valid
      if(.not. compatible .and. present(message)) then
         message = 'The first interval is not valid: ' // aug1%string // make_int_string(aug1%status)
      end if

      _RETURN_UNLESS(compatible)

      aug2 = AugmentedInterval(interval2)
      compatible = aug2%valid
      if(.not. compatible .and. present(message)) then
         message = 'The second interval is not valid: ' // aug2%string // make_int_string(aug2%status)
      end if
      _RETURN_UNLESS(compatible)

      if(present(offset)) then
         aug0 = AugmentedInterval(offset)
         compatible = aug0%valid
      end if

      if(.not. compatible .and. present(message)) then
         message = 'The offset is not valid: '
         if(allocated(aug0%string)) message = message // aug0%string // make_int_string(aug0%status)
      end if
      _RETURN_UNLESS(compatible)

      _ASSERT(.not. aug2%all_zero, 'The second interval cannot be 0.')

      if(present(offset)) then
         call intervals_are_compatible(aug0, aug2, compatible, message_, _RC)
      end if

      if(.not. compatible .and. present(message)) then
         message = 'The offset and second interval are not compatible: ' 
         if(allocated(aug0%string) .and. allocated(aug2%string)) message = message // aug0%string // ', ' // aug2%string
         message = message // message_
      end if
      _RETURN_UNLESS(compatible)

      _RETURN_IF(compatible .and. (aug1%interval == aug2%interval))

      call intervals_are_compatible(aug1, aug2, compatible, message_, _RC)
      if(.not. compatible .and. present(message)) then
         message = 'The first and second interval are not compatible: '
         if(allocated(aug1%string) .and. allocated(aug2%string)) message = message // aug1%string // ', ' // aug2%string
         message = message // ' ' // message_
      end if
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

   subroutine intervals_are_compatible(aug1, aug2, compatible, message, rc)
      type(AugmentedInterval), intent(in) :: aug1
      type(AugmentedInterval), intent(in) :: aug2
      logical, intent(out) :: compatible
      character(len=:), allocatable, intent(out) :: message
      integer, optional, intent(out) :: rc
      integer :: status
      type(AugmentedInterval) :: augmod

      _ASSERT(.not. aug2%all_zero, 'The second interval is all zero.')
      compatible = aug1%all_zero .or. aug1%interval == aug2%interval
      _RETURN_IF(compatible)

      compatible = (aug1%yy_mm_only .and. aug2%yy_mm_only) .or. (aug1%d_s_only .and. aug2%d_s_only)
      if(.not. compatible) message = 'The intervals do not have the same form.'
      _RETURN_UNLESS(compatible)

      augmod = AugmentedInterval(mod(aug1%interval, aug2%interval))
      _ASSERT(augmod%valid, 'Unable to perform modulo operation')
      compatible = augmod%all_zero
      if(.not. compatible) message = 'The second interval does not divide the first interval evenly.'
      _RETURN(_SUCCESS)

   end subroutine intervals_are_compatible
   
   function make_int_string(n) result(string)
      character(len=:), allocatable :: string
      integer, intent(in) :: n
      character(len=64) :: string_

      write(string_, fmt='(G0)')
      string = trim(adjustl(string_))

   end function make_int_string

!   subroutine intervals_are_compatible(interval1, interval2, compatible, message, rc)
!      type(ESMF_TimeInterval), intent(in) :: interval1
!      type(ESMF_TimeInterval), intent(in) :: interval2
!      logical, intent(out) :: compatible
!      character(len=:), allocatable, intent(out) :: message
!      integer, optional, intent(out) :: rc
!      integer :: status
!      integer(kind=I4), allocatable :: fields1(:), fields2(:)
!
!      compatible = .TRUE.
!      _RETURN_IF(all_zero(interval1) .or. interval1 == interval2)
!
!      call is_valid(interval1, compatible, _RC)
!      if(.not. compatible) message = 'The first interval cannot be compared.'
!      _RETURN_UNLESS(compatible)
!      
!      call is_valid(interval2, compatible, _RC)
!      if(.not. compatible) message = 'The second interval cannot be compared.'
!      _RETURN_UNLESS(compatible)
!
!      fields1 = get_fields(interval1, _RC)
!      fields2 = get_fields(interval2, _RC)
!
!      compatible = all(fields1(DAY:)==0) .eqv. all(fields2(DAY:)==0)
!      if(.not. compatible) message = 'The intervals do not have the same form.'
!      _RETURN_UNLESS(compatible)
!
!      compatible = all_zero(mod(interval1, interval2), _RC)
!      if(.not. compatible) message = 'The second interval does not divide the first interval evenly.'
!      _RETURN(_SUCCESS)
!
!   end subroutine intervals_are_compatible

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

!      logical function only_yy_mm(f)
!         integer(kind=I4), intent(in) :: f(:)
!         integer, parameter :: DAY = 3
!
!         only_yy_mm = all(f(DAY:) == 0)
!
!      end function only_yy_mm
!
!      logical function abs_le(interval1, interval2)
!         type(ESMF_TimeInterval), intent(in) :: interval1, interval2
!
!         abs_le = ESMF_TimeIntervalAbsValue(interval1) <= ESMF_TimeIntervalAbsValue(interval2)
!         abs_le = interval1 <= interval2
!
!      end function abs_le
!         
!      logical function can_compare(f1, f2)
!         integer(kind=I4), intent(in) :: f1(:), f2(:)
!         
!         can_compare = only_yy_mm(f1) .and. only_yy_mm(f2) .or. no_yy_mm(f1) .and. no_yy_mm(f2)
!
!      end function can_compare
!
!      logical function no_yy_mm(f)
!         integer(kind=I4), intent(in) :: f(:)
!         integer, parameter :: DAY = 3
!         
!         no_yy_mm = all(f(:DAY-1) == 0)
!
!      end function no_yy_mm
!
!      function dt_args(interval, rc) result(a)
!         integer(kind=I4) :: a(SZ_DT)
!         type(ESMF_TimeInterval), intent(in) :: interval
!         integer, optional, intent(out) :: rc
!         integer(kind=I4) :: yy, mm, d, h, m, s, ms, us, ns
!         integer :: status
!
!         call ESMF_TimeIntervalGet(interval, mm=mm, d=d, h=h, m=m, s=s, ms=ms, us=us, ns=ns, _RC)
!         a = [0, mm, d, h, m, s, ms, us, ns]
!         _RETURN(_SUCCESS)
!
!      end function dt_args

end module mapl3g_ESMF_Time_Utilities
