#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_RangeMod
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use MAPL_ExceptionHandling
  implicit none
  private

  public :: MAPL_Range

  interface MAPL_Range
    module procedure MAPL_Range_REAL64
    module procedure MAPL_Range_REAL32
  end interface

contains

  ! Analog of range() procedure in Python for constructing
  ! an arithmetic sequence of values.  Introducing
  ! into MAPL to enforce consistent roundoff within
  ! various parts of the model that generate lat and lon
  ! coordinates.

  function MAPL_Range_REAL64(x0, x1, n, conversion_factor,rc) result(range)
     real(kind=REAL64), allocatable :: range(:)
     real(kind=REAL64), intent(in) :: x0
     real(kind=REAL64), intent(in) :: x1
     integer, intent(in) :: n
     real(kind=REAL64), optional, intent(in) :: conversion_factor
     integer, optional, intent(out) :: rc

     integer :: i
     real(kind=REAL64) :: delta

     _ASSERT(((n /= 1) .or. (x0 == x1)),'needs informative message')
     allocate(range(n))
     
     range(1) = x0
     range(n) = x1

     if (n > 1) then
        delta = (x1 - x0)/(n-1)
        do i = 2, n-1
           range(i) = x0 + (i-1)*delta
        end do
     end if

     if (present(conversion_factor)) then
        range = range * conversion_factor
     end if

     _RETURN(_SUCCESS)
        
  end function MAPL_Range_REAL64

  function MAPL_Range_REAL32(x0, x1, n, conversion_factor,rc) result(range)
     real(kind=REAL64), allocatable :: range(:)
     real(kind=REAL32), intent(in) :: x0
     real(kind=REAL32), intent(in) :: x1
     integer, intent(in) :: n
     real(kind=REAL64), optional, intent(in) :: conversion_factor
     integer, optional, intent(out) :: rc

     integer :: i
     real(kind=REAL64) :: delta

     _ASSERT((n /= 1) .or. (x0 == x1),'needs informative message')
     allocate(range(n))
     
     range(1) = x0
     range(n) = x1

     if (n > 1) then
        delta = real(x1 - x0,REAL64)/(n-1)
        do i = 2, n-1
           range(i) = x0 + (i-1)*delta
        end do
     end if

     if (present(conversion_factor)) then
        range = range * conversion_factor
     end if

     _RETURN(_SUCCESS)
        
  end function MAPL_Range_REAL32

end module MAPL_RangeMod
