module mapl_RegridderSpec
   implicit none
   private

   public :: RegridderSpec

   type, abstract :: RegridderSpec
      private
!!$      integer :: grid_id_in
!!$      integer :: grid_id_out
   contains
      procedure(I_equal_to), deferred :: equal_to
      generic :: operator(==) => equal_to
!!$
!!$      procedure :: set_grid_id_in
!!$      procedure :: set_grid_id_out
!!$      procedure :: get_grid_id_in
!!$      procedure :: get_grid_id_outn
   end type RegridderSpec

   abstract interface
      logical function I_equal_to(this, other)
         import RegridderSpec
         class(RegridderSpec), intent(in) :: this
         class(RegridderSpec), intent(in) :: other
      end function I_equal_to
   end interface

end module mapl_RegridderSpec
