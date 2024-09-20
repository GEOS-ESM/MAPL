#include "MAPL_Generic.h"

module mapl3g_VerticalRegridMethod

   implicit none
   private
   
   public :: VerticalRegridMethod_Flag
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)

   type :: VerticalRegridMethod_Flag
      private
      integer :: id = -1
   end type VerticalRegridMethod_Flag

   interface operator(==)
      procedure :: equal_to
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
   end interface operator(/=)

   type(VerticalRegridMethod_Flag), parameter :: VERTICAL_REGRID_UNKNOWN = VerticalRegridMethod_Flag(-1)
   type(VerticalRegridMethod_Flag), parameter :: VERTICAL_REGRID_LINEAR = VerticalRegridMethod_Flag(1)
   type(VerticalRegridMethod_Flag), parameter :: VERTICAL_REGRID_CONSERVATIVE = VerticalRegridMethod_Flag(2)

contains

   pure logical function equal_to(a, b)
      type(VerticalRegridMethod_Flag), intent(in) :: a, b
      equal_to = (a%id == b%id)
   end function equal_to

   pure logical function not_equal_to(a, b)
      type(VerticalRegridMethod_Flag), intent(in) :: a, b
      not_equal_to = .not. (a==b)
   end function not_equal_to

end module mapl3g_VerticalRegridMethod
