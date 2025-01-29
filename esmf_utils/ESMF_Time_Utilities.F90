#include "MAPL_Generic.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   implicit none (type, external)
!   private !wdb fixme deleteme should this be private

   public :: zero_time_interval
   public :: intervals_are_compatible
   public :: times_and_intervals_are_compatible

   type :: IntervalUnits
      integer(kind=I4) :: years = 0
      integer(kind=I4) :: months = 0
      integer(kind=I4) :: days = 0
      integer(kind=I4) :: hours = 0
      integer(kind=I4) :: minutes = 0
      integer(kind=I4) :: seconds = 0
      integer(kind=I4) :: milliseconds = 0
      integer(kind=I4) :: microseconds = 0
      integer(kind=I4) :: nanoseconds = 0
      integer :: size = 9
      logical :: invalid = .TRUE.
   contains
      procedure :: has_only_yy_mm
      procedure :: has_no_yy_mm
      procedure :: as_array
   end type IntervalUnits

   interface IntervalUnits
      module procedure :: construct_interval_units
   end interface IntervalUnits

   interface zero_time_interval
      module procedure :: get_zero
   end interface zero_time_interval

   integer, parameter :: NUM_DATETIME_FIELDS = 6

   ! This value should not be accessed directly. Use get_zero() instead.
   ! There is no constructor for ESMF_TimeInterval, so the value cannot be initialized
   ! at construction. The get_zero() function initializes the value the first time
   ! and returns a pointer to the value.
   type(ESMF_TimeInterval), target :: ZERO_TI

contains

!   These combinations do not work.
!   Interval1   |   Interval2
!   mm          |   s
!   mm          |   m
!   mm          |   h
!   mm          |   d
!   yy          |   s
!   yy          |   m
!   yy          |   h
!   yy          |   d
   ! If the smaller interval includes nonzero year or month units,
   ! only the year and month units of the larger interval can be nonzero.
   ! This function does not handle DST and leap seconds.
   ! If DST is used and the smaller interval has units of days,
   ! the larger interval can only have units of years, months, and days.
   ! If leap seconds are included, both intervals must have units of seconds only
   ! if one interval has units of seconds.
   logical function can_compare_intervals(larger, smaller, rc)
      type(ESMF_TimeInterval), intent(inout) :: larger
      type(ESMF_TimeInterval), intent(inout) :: smaller
      integer, optional, intent(out) :: rc
      integer :: status
      integer(kind=I4) :: lyy, lmm, ld, lh, lm, ls
      integer(kind=I4) :: syy, smm, sd, sh, sm, ss
      logical :: smaller_has_no_yy_mm
      logical :: larger_has_only_yy_mm

      can_compare_intervals = .FALSE.
      larger_units = IntervalUnits(larger)
      smaller_units = IntervalUnits(smaller)
      call ESMF_TimeIntervalGet(larger, yy=lyy, mm=lmm, d=ld, h=lh, m=lm, s=ls, _RC)
      call ESMF_TimeIntervalGet(smaller, yy=syy, mm=smm, d=sd, h=sh, m=sm, s=ss, _RC)
      larger_has_yy_or_mm = lyy /= 0 .or. lmm /= 0

      smaller_has_no_yy_mm = all([syy, smm]==0) 
      larger_has_only_yy_mm = all([ld, lh, lm, ls]==0)
      _RETURN_UNLESS(smaller_has_no_yy_mm .or. larger_has_only_yy_mm)
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

   ! First, it must be possible to compare them, based on their nonzero units.
   ! Second, the smaller interval must divide the larger interval evenly based on the
   ! overloaded mod function for ESMF_TimeInterval objects and it assumes they have the same sign.
   logical function intervals_are_compatible(larger, smaller, rc) result(compatible)
      type(ESMF_TimeInterval), intent(inout) :: larger
      type(ESMF_TimeInterval), intent(inout) :: smaller
      integer, optional, intent(out) :: rc
      integer :: status

      _ASSERT(smaller /= get_zero(), 'The smaller unit must be nonzero.')
      associate(abs_larger => ESMF_TimeIntervalAbsValue(larger), abs_smaller => ESMF_TimeIntervalAbsValue(smaller))
         compatible = abs_larger >= abs_smaller
         _RETURN_UNLESS(compatible)
         compatible = can_compare_intervals(larger, smaller, _RC)
         _RETURN_UNLESS(compatible)
         compatible = mod(abs_larger, abs_smaller) == get_zero()
      end associate

      _RETURN(_SUCCESS)

   end function intervals_are_compatible

   ! The magnitude of the first interval should be greater than or equal to the magnitude of the second interval.
   logical function times_and_intervals_are_compatible(interval1, time1, interval2, time2, rc) result(compatible)
      type(ESMF_Time), intent(inout) :: time1
      type(ESMF_Time), intent(inout) :: time2
      type(ESMF_TimeInterval), target, intent(inout) :: interval1
      type(ESMF_TimeInterval), target, intent(inout) :: interval2
      integer, optional, intent(inout) :: rc
      integer :: status
      type(ESMF_TimeInterval) :: absdiff

      compatible = intervals_are_compatible(interval1, interval2, _RC)
      _RETURN_UNLESS(compatible)
      absdiff = ESMF_TimeIntervalAbsValue(time1 - time2)
      compatible = mod(absdiff, ESMF_TimeIntervalAbsValue(interval2)) == get_zero()
      _RETURN(_SUCCESS)

   end function times_and_intervals_are_compatible

   logical function is_fraction_of_day(this)
      type(IntervalUnits) :: this
      integer(kind=ESMF_KIND_I8), parameter :: MULTIPLIERS(7) = &
         [0, 0, 1, 24, 60, 60, 1000, 1000, 1000]
      integer :: i
      integer(kind=I4) :: array
      integer(kind=I8) :: ns

      is_fraction_of_day = .not. this%invalid .and. this%has_no_yy_mm()
      if(.not. is_fraction_of_day) return
      array = this%as_array()
      ns = 0
      do i = 3, size(array)
         if(array(i) == 0) cycle
         ns = ns + array(i) * product(MULTIPLIERS(i:))
      end do
      is_fraction
      is_fraction_of_day = mod(unit_day, ESMF_TimeIntervalAbsValue(interval, _RC)) == get_zero()

   end function is_fraction_of_day

   function construct_interval_units(interval) result(units)
      type(IntervalUnits) :: units
      type(ESMF_TimeInterval) :: interval
      integer :: status

      call ESMF_TimeIntervalGet(interval, yy=units%years, mm=units%months, d=units%days, &
         & h=units%hours, m=units%minutes, s=units%seconds, &
         & ms=units%milliseconds, us=units%microseconds, ns=units%nanoseconds, rc=status)
      if(status /= ESMF_SUCCESS) return
      units%invalid = .FALSE.

   end function construct_interval_units

   function as_array(this) result(array)
      integer(kind=I4) :: array(this%size)
      type(IntervalUnits), intent(in) :: this

      array = [this%years, this%months, this%days, this%hours, this%minutes, this%seconds, &
         this%milliseconds, this%microseconds, this%nanoseconds]

   end function as_array

   logical function has_only_yy_mm(this)
      type(IntervalUnits), intent(in) :: this
      integer(kind=I4) :: array(this%size)

      array = this%as_array()
      has_only_yy_mm = all(array(3:) == 0)

   end function has_only_yy_mm

   logical function has_no_yy_mm(this)
      type(IntervalUnits), intent(in) :: this
      integer(kind=I4) :: array(this%size)

      array = this%as_array()
      has_no_yy_mm = all(array(1:2) == 0)

   end function has_no_yy_mm

end module mapl3g_ESMF_Time_Utilities
