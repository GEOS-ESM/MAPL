module MAPL_MinMaxMod
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: IntegerMinMax
   public :: RealMinMax
   public :: Real64MinMax

   type IntegerMinMax
      integer :: min
      integer :: max
    end type IntegerMinMax

   type RealMinMax
      real(kind=REAL32) :: min
      real(kind=REAL32) :: max
   contains
      procedure :: equal_real
      procedure :: not_equal_real
      generic :: operator(==) => equal_real
      generic :: operator(/=) => not_equal_real
    end type RealMinMax

   type Real64MinMax
      real(kind=REAL64) :: min
      real(kind=REAL64) :: max
    end type Real64MinMax

contains


   logical function equal_real(a, b)
      class (RealMinMax), intent(in) :: a
      type (RealMinMax), intent(in) :: b

      equal_real = (a%min == b%min) .and. (a%max == b%max)
   end function equal_real


   logical function not_equal_real(a, b)
      class (RealMinMax), intent(in) :: a
      type (RealMinMax), intent(in) :: b

      not_equal_real = .not. (a==b)

   end function not_equal_real

end module MAPL_MinMaxMod
