module oomph_VerticalStaggerLoc
   implicit none
   private

   public :: VerticalStaggerLoc
   public :: V_STAGGER_LOC_NONE
   public :: V_STAGGER_LOC_EDGE
   public :: V_STAGGER_LOC_CENTER
   
   integer, parameter :: INVALID = -1

   type :: VerticalStaggerLoc
      private
      integer :: i = INVALID
   contains
      procedure :: equal_to
      procedure :: not_equal_to
      generic :: operator(==) => equal_to
      generic :: operator(/=) => not_equal_to
   end type VerticalStaggerLoc

   type(VerticalStaggerLoc) :: V_STAGGER_LOC_NONE = VerticalStaggerLoc(0)
   type(VerticalStaggerLoc) :: V_STAGGER_LOC_EDGE = VerticalStaggerLoc(1)
   type(VerticalStaggerLoc) :: V_STAGGER_LOC_CENTER = VerticalStaggerLoc(2)

   
contains


   pure logical function equal_to(this, other)
      class(VerticalStaggerLoc), intent(in) :: this
      type(VerticalStaggerLoc), intent(in) :: other
      equal_to = this%i == other%i
   end function equal_to
   
   pure logical function not_equal_to(this, other)
      class(VerticalStaggerLoc), intent(in) :: this
      type(VerticalStaggerLoc), intent(in) :: other
      not_equal_to = .not. (this == other)
   end function not_equal_to
   
   
end module oomph_VerticalStaggerLoc
