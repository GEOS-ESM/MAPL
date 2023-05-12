module mapl_RegridderParam
   implicit none
   private

   public :: RegridderParam

   type, abstract :: RegridderParam
   contains
      procedure(I_equal_to), deferred :: equal_to
      generic :: operator(==) => equal_to
   end type RegridderParam

   abstract interface
      logical function I_equal_to(this, other)
         import RegridderParam
         class(RegridderParam), intent(in) :: this
         class(RegridderParam), intent(in) :: other
      end function I_equal_to
   end interface

end module mapl_RegridderParam
