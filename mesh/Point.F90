module sf_Point
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none(type,external)
   private

   public :: Point
   public :: linear

   type :: Point
      real(kind=REAL64) :: latitude, longitude
   end type Point

contains

   function linear(p1, p2, r1, r2) result(p)
      type(Point) :: p
      type(Point), intent(in) :: p1, p2
      real(kind=REAL64), intent(in) :: r1, r2

      p%longitude = linear_1(p1%longitude, p2%longitude, r1)
      p%latitude = linear_1(p1%latitude, p2%latitude, r2)

   end function linear

   function linear_1(x1, x2, r) result(x)
      real(kind=REAL64) :: x
      real(kind=REAL64), intent(in) :: x1, x2
      real(kind=REAL64), intent(in) :: r

      x = x1 + r *(x2-x1)
   end function linear_1

end module sf_Point
   
