#include "MAPL_Generic.h"

module mapl3g_newActualConnectionPt
   use mapl3g_newVirtualConnectionPt
   use mapl_KeywordEnforcer
   implicit none
   private
  
   public :: newActualConnectionPt
   public :: operator(<)
   public :: operator(==)

   type :: newActualConnectionPt
      private
      type(newVirtualConnectionPt) :: v_pt
   end type newActualConnectionPt

   ! Constructors
   interface newActualConnectionPt
      module procedure new_newActualPt_from_v_pt
   end interface newActualConnectionPt

   interface operator(<)
      module procedure less_than
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

contains

   function new_newActualPt_from_v_pt(v_pt) result(gc_pt)
      type(newActualConnectionPt) :: gc_pt
      type(newVirtualConnectionPt), intent(in) :: v_pt

      gc_pt%v_pt = v_pt

   end function new_newActualPt_from_v_pt

   logical function less_than(lhs, rhs)
      type(newActualConnectionPt), intent(in) :: lhs
      type(newActualConnectionPt), intent(in) :: rhs
      less_than = lhs%v_pt < rhs%v_pt
   end function less_than

   logical function equal_to(lhs, rhs)
      type(newActualConnectionPt), intent(in) :: lhs
      type(newActualConnectionPt), intent(in) :: rhs

      equal_to = .not. ((lhs < rhs) .or. (rhs < lhs))
      
   end function equal_to

end module mapl3g_newActualConnectionPt
