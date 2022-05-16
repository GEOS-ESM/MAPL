module oomph_HorizontalStaggerLoc
   implicit none
   private

   public :: HorizontalStaggerLoc
   public :: H_STAGGER_LOC_NONE
   public :: H_STAGGER_LOC_CENTER
   public :: H_STAGGER_LOC_TILE

   integer, parameter :: INVALID = -1

   ! Users should not be able to invent their own staggering, but we
   ! need to be able to declare type components of this type, so we
   ! cannot simply make the type private.  Instead we give it a
   ! default value that is invalid.  This class does not check the
   ! value, but higher level logic should check that returned values
   ! are of one of the defined parameters.
  
   type :: HorizontalStaggerLoc
      private
      integer :: i = INVALID
   contains
      procedure :: equal_to
      procedure :: not_equal_to
      generic :: operator(==) => equal_to
      generic :: operator(/=) => not_equal_to
   end type HorizontalStaggerLoc

   type(HorizontalStaggerLoc) :: H_STAGGER_LOC_NONE = HorizontalStaggerLoc(0)
   type(HorizontalStaggerLoc) :: H_STAGGER_LOC_CENTER = HorizontalStaggerLoc(2)
   type(HorizontalStaggerLoc) :: H_STAGGER_LOC_TILE = HorizontalStaggerLoc(3)
   
contains


   pure logical function equal_to(this, other)
      class(HorizontalStaggerLoc), intent(in) :: this
      type(HorizontalStaggerLoc), intent(in) :: other
      equal_to = this%i == other%i
   end function equal_to
   
   pure logical function not_equal_to(this, other)
      class(HorizontalStaggerLoc), intent(in) :: this
      type(HorizontalStaggerLoc), intent(in) :: other
      not_equal_to = .not. (this == other)
   end function not_equal_to
   
   
end module oomph_HorizontalStaggerLoc
