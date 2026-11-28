module mapl_VerticalGridSpec_mod
   implicit none(type,external)
   private
   
   public :: VerticalGridSpec
   
   type, abstract :: VerticalGridSpec
      private
   contains
      procedure(I_make_grid), deferred :: make_grid
   end type VerticalGridSpec

   abstract interface
      function I_make_grid(this, rc) result(grid)
         use mapl_VerticalGrid_mod, only: VerticalGrid
         import VerticalGridSpec
         implicit none
         class(VerticalGrid), allocatable :: grid
         class(VerticalGridSpec), intent(in) :: this
         integer, optional, intent(out) :: rc
      end function I_make_grid
   end interface

end module mapl_VerticalGridSpec_mod
