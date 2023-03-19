module mapl3g_VerticalDimSpec
   use mapl3g_UngriddedDimSpec
   implicit none
   private
  
   public :: VerticalDimSpec
  
   public :: VERTICAL_DIM_NONE
   public :: VERTICAL_DIM_CENTER
   public :: VERTICAL_DIM_EDGE


   type :: VerticalDimSpec
      private
      integer :: id = -1
   end type VerticalDimSpec

   type(VerticalDimSpec), parameter :: VERTICAL_DIM_NONE = VerticalDimSpec(0)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_CENTER = VerticalDimSpec(1)
   type(VerticalDimSpec), parameter :: VERTICAL_DIM_EDGE = VerticalDimSpec(2)

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)
      
contains


   elemental logical function equal_to(a, b)
      type(VerticalDimSpec), intent(in) :: a, b
      equal_to = a%id == b%id
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(VerticalDimSpec), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

  
end module mapl3g_VerticalDimSpec
