#include "MAPL_Generic.h"

module mapl3g_VerticalRegridMethod

   implicit none
   private
   
   public :: VerticalRegridMethod
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)

   type :: VerticalRegridMethod
      private
      integer :: id = -1
   end type VerticalRegridMethod

   interface operator(==)
      procedure :: equal_to
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
   end interface operator(/=)

   type(VerticalRegridMethod), parameter :: VERTICAL_REGRID_UNKNOWN = VerticalRegridMethod(-1)
   type(VerticalRegridMethod), parameter :: VERTICAL_REGRID_LINEAR = VerticalRegridMethod(1)
   type(VerticalRegridMethod), parameter :: VERTICAL_REGRID_CONSERVATIVE = VerticalRegridMethod(2)

contains

   elemental logical function equal_to(a, b)
      type(VerticalRegridMethod), intent(in) :: a, b
      equal_to = (a%id == b%id)
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(VerticalRegridMethod), intent(in) :: a, b
      not_equal_to = .not. (a==b)
   end function not_equal_to

end module mapl3g_VerticalRegridMethod
