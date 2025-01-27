!wdb fixme deleteme should this be different include file
#include "MAPL_Generic.h"
module mapl3g_ESMF_Time_Utilities
   use esmf, I4 => ESMF_KIND_I4
   use mapl_ErrorHandling
   implicit none !wdb fixme deleteme hsould replace this with new implicit none
!   private !wdb fixme deleteme should this be private

   public :: zero_time_interval
   public :: intervals_are_compatible
   public :: times_and_intervals_are_compatible
   public :: zero_time_interval

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

   function units_in(interval, rc) result(units)
      logical :: units(NUM_DATETIME_FIELDS)
      type(ESMF_TimeInterval), intent(inout) :: interval
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: yy, mm, dd, h, m, s
      integer(kind=I4) :: a(NUM_DATETIME_FIELDS)

      call ESMF_TimeIntervalGet(interval, yy=a(1), mm=a(2), dd=a(3), h=a(4), m=a(5), s=a(6), _RC)
      units = a /= 0
      _RETURN(_SUCCESS)

   end function units_in

   logical function can_compare_intervals(larger, smaller, rc)
      type(ESMF_TimeInterval), intent(inout) :: larger
      type(ESMF_TimeInterval), intent(inout) :: smaller
      integer, optional, intent(out) :: rc
      integer :: status
      logical, allocatable :: has_units(:)

      can_compare_intervals = .FALSE.
      has_units = units_in(larger)
      _RETURN_UNLESS(all(has_units(1:2) == 0) 
      has_units = units_in(smaller)
      _RETURN_UNLESS(all(has_units(1:2) == 0) 
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
      type(ESMF_TimeInterval), intent(in) :: larger
      type(ESMF_TimeInterval), intent(in) :: smaller
      logical, intent(in) :: dst
      logical, intent(in) :: leap_seconds
      integer, optional, intent(out) :: rc
      integer :: status

      associate(abs_larger => ESMF_TimeIntervalAbsValue(larger), abs_smaller => ESMF_TimeIntervalAbsValue(smaller))
         compatible = ESMF_TimeIntervalAbsValue(larger) >= ESMF_TimeIntervalAbsValue(smaller)
         _RETURN_UNLESS(compatible)
         compatible = can_compare_intervals(larger, smaller, _RC)
         _RETURN_UNLESS(compatible)
         compatible = mod(abs_larger, abs_smaller) == get_zero()
      end associate

      _RETURN(_SUCCESS)

   end function intervals_are_compatible

   logical function times_and_intervals_are_compatible(time1, time2, larger, smaller, rc) result(compatible)
      type(ESMF_Time), intent(in) :: time1
      type(ESMF_Time), intent(in) :: time2
      type(ESMF_TimeInterval), target, intent(in) :: larger
      type(ESMF_TimeInterval), target, intent(in) :: smaller
      integer, optional, intent(in) :: rc
      integer :: status
      logical :: compatible
      type(ESMF_TimeInterval), pointer :: interval => null()

      compatible = intervals_are_compatible(larger, smaller, _RC)
      _RETURN_UNLESS(compatible)
      compatible = mod(ESMF_TimeIntervalAbsValue(time1 - time2), ESMF_TimeInterval(smaller)) == get_zero()
      _RETURN(_SUCCESS)

   end function times_and_intervals_are_compatible

end module mapl3g_ESMF_Time_Utilities

!   function minimum_unit(interval, rc) result(minunit)
!      integer(kind=UNIT_KIND) :: minunit
!      integer, optional, intent(out) :: rc
!      integer : status
!      logical, allocatable :: has_unit(:)
!      integer(kind=UNIT_KIND) :: i
!
!      minunit = NO_UNIT
!      has_unit = units_in(interval, _RC)
!      do i = 1, size(has_unit)
!         if(.not. has_unit(i)) cycle
!         minunit = i
!         exit
!      end do
!
!   end function minimum_unit
!      
!   function maximum_unit(interval, rc) result(maxunit)
!      integer(kind=UNIT_KIND) :: maxunit
!      integer, optional, intent(out) :: rc
!      integer : status
!      logical, allocatable :: has_unit(:)
!      integer(kind=UNIT_KIND) :: i
!
!      maxunit = NO_UNIT
!      has_unit = units_in(interval, _RC)
!      do i = size(has_unit), 1, -1
!         if(.not. has_unit(i)) cycle
!         maxunit = i
!         exit
!      end do
!
!   end function maximum_unit
!
!   function construct_dts_from_array(array) result(dts)
!      type(DateTimeStruct) :: dts
!      integer(kind=I4), intent(in) :: array(:)
!
!      _ASSERT(size(array) >= NUM_DATETIME_FIELDS)
!      dts%year = array(1)
!      dts%month = array(2)
!      dts%day = array(3)
!      dts%hour = array(4)
!      dts%minute = array(5)
!      dts%second = array(6)
!
!   end function construct_dts_from_array
!
!   function datetime_struct_to_array(this) result(array)
!      integer(kind=I4) :: array(NUM_DATETIME_FIELDS)
!      class(DateTimeStruct), intent(in) :: this
!
!      array = [this%year, this%month, this%day, this%hour, this%minute, this%second]
!
!   end function

!   logical function has_months(interval, rc)
!      type(ESMF_TimeInterval), intent(in) :: interval
!      integer, optional, intent(out) :: rc 
!      integer :: status
!      integer :: mm
!
!      call ESMF_TimeIntervalGet(interval, mm=mm, _RC)
!      has_months = mm /= 0
!      _RETURN(_SUCCESS)
!
!   end function has_months(interval, rc)
!
!   logical function has_years(interval, rc)
!      type(ESMF_TimeInterval), intent(in) :: interval
!      integer, optional, intent(out) :: rc 
!      integer :: status
!      integer :: yy
!
!      call ESMF_TimeIntervalGet(interval, yy=yy, _RC)
!      has_years = yy /= 0
!      _RETURN(_SUCCESS)
!
!   end function has_years(interval, rc)

!   type :: DateTimeStruct
!      integer(kind=I4) :: year = 0
!      integer(kind=I4) :: month = 0
!      integer(kind=I4) :: days = 0
!      integer(kind=I4) :: hour = 0
!      integer(kind=I4) :: minute = 0
!      integer(kind=I4) :: second = 0
!   contains
!      procedure :: to_array => datetime_struct_to_array
!   end type DateTimeStruct
!
!   interface DateTimeStruct
!      module procedure :: construct_dts_from_array
!   end interface DateTimeStruct
!      integer(kind=I4) :: array
!      type(DateTimeStruct) :: larger_struct
!      type(DateTimeStruct) :: smaller_struct
!
!      call ESMF_TimeIntervalGet(larger, yy=array(1), mm=array(2), dd=array(3), &
!         h=array(4), m=array(5), s=array(6), _RC)
!      larger_struct = DateTimeStruct(array)
!      call ESMF_TimeIntervalGet(smaller, yy=array(1), mm=array(2), dd=array(3), &
!         h=array(4), m=array(5), s=array(6), _RC)
!      can_compare_intervals = all([maximum_unit(smaller), minimum_unit(larger)] > DAYS_UNIT &
!                              & .or. [minimum_unit(smaller), maximum_unit(larger)] < MONTHS_UNIT)
