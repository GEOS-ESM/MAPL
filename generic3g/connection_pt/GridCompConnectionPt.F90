#include "MAPL_Generic.h"

module mapl3g_GridCompConnectionPt
   use mapl3g_newVirtualConnectionPt
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private
  
   public :: GridCompConnectionPt
   public :: operator(<)
   public :: operator(==)

   type, extends(newVirtualConnectionPt) :: GridCompConnectionPt
      private
   end type GridCompConnectionPt

   ! Constructors
   interface GridCompConnectionPt
      module procedure new_GridCompPt_from_v_pt
   end interface GridCompConnectionPt

   interface operator(<)
      module procedure less_than
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

contains

   function new_GridCompPt_from_v_pt(v_pt) result(gc_pt)
      type(GridCompConnectionPt) :: gc_pt
      type(newVirtualConnectionPt), intent(in) :: v_pt

      gc_pt%newVirtualConnectionPt = v_pt

   end function new_GridCompPt_from_v_pt

   logical function less_than(lhs, rhs)
      type(GridCompConnectionPt), intent(in) :: lhs
      type(GridCompConnectionPt), intent(in) :: rhs
      less_than = lhs%newVirtualConnectionPt < rhs%newVirtualConnectionPt
   end function less_than

   logical function equal_to(lhs, rhs)
      type(GridCompConnectionPt), intent(in) :: lhs
      type(GridCompConnectionPt), intent(in) :: rhs

      equal_to = .not. ((lhs < rhs) .or. (rhs < lhs))
      
   end function equal_to

end module mapl3g_GridCompConnectionPt
