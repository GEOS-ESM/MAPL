#include "MAPL_Generic.h"

module mapl3g_BasicVerticalGrid
   use mapl3g_VerticalGrid
   use mapl_ErrorHandling
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoCreate
   use esmf, only: ESMF_InfoSet
   implicit none
   private
   public :: BasicVerticalGrid

   type, extends(VerticalGrid) :: BasicVerticalGrid
      private
      integer :: num_levels = 0
   contains
      procedure :: get_num_levels
   end type

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)

   interface BasicVerticalGrid
      module procedure new_BasicVerticalGrid
   end interface BasicVerticalGrid

contains

   function new_BasicVerticalGrid(num_levels) result(vertical_grid)
      type(BasicVerticalGrid) :: vertical_grid
      integer, intent(in) :: num_levels
      vertical_grid%num_levels = num_levels
   end function

   function get_num_levels(this) result(num_levels)
      integer :: num_levels
      class(BasicVerticalGrid), intent(in) :: this
      num_levels = this%num_levels
   end function
      
   elemental logical function equal_to(a, b)
      type(BasicVerticalGrid), intent(in) :: a, b
      equal_to = a%num_levels == b%num_levels
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(BasicVerticalGrid), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

end module mapl3g_BasicVerticalGrid
