#include "MAPL_Generic.h"
module mapl3g_VerticalGeom
   implicit none
   private
   public :: VerticalGeom

   type VerticalGeom
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

   interface VerticalGeom
      module procedure new_VerticalGeom
   end interface VerticalGeom

contains

   function new_VerticalGeom(num_levels) result(vertical_geom)
      type(VerticalGEOM) :: vertical_geom
      integer, intent(in) :: num_levels
      vertical_geom%num_levels = num_levels
   end function

   function get_num_levels(this) result(num_levels)
      integer :: num_levels
      class(VerticalGeom), intent(in) :: this
      num_levels = this%num_levels
   end function
      
   elemental logical function equal_to(a, b)
      type(VerticalGeom), intent(in) :: a, b
      equal_to = a%num_levels == b%num_levels
   end function equal_to

   elemental logical function not_equal_to(a, b)
      type(VerticalGeom), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

end module mapl3g_VerticalGeom
