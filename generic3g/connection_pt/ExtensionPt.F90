#include "MAPL_Generic.h"

module mapl3g_ExtensionConnectionPt
   use mapl3g_newVirtualConnectionPt
   use mapl3g_GridCompConnectionPt
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private
  
   public :: ExtensionConnectionPt
   public :: operator(<)
   public :: operator(==)

   type, extends(GridCompConnectionPt) :: ExtensionConnectionPt
      private
      integer :: label = 0
   end type ExtensionConnectionPt

   ! Constructors
   interface ExtensionConnectionPt
      module procedure new_ExtensionPt_from_v_pt
      module procedure new_ExtensionPt_from_gc_pt
   end interface ExtensionConnectionPt

   interface operator(<)
      module procedure less_than
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

contains

   function new_ExtensionPt_from_gc_pt(gc_pt, unusable, label) result(ext_pt)
      type(ExtensionConnectionPt) :: ext_pt
      type(GridCompConnectionPt), intent(in) :: gc_pt
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: label

      ext_pt%GridCompConnectionPt = gc_pt
      if (present(label)) ext_pt%label = label
      
      _UNUSED_DUMMY(unusable)
   end function new_ExtensionPt_from_gc_pt

   function new_ExtensionPt_from_v_pt(v_pt, unusable, label) result(ext_pt)
      type(ExtensionConnectionPt) :: ext_pt
      type(newVirtualConnectionPt), intent(in) :: v_pt
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: label

      ext_pt = ExtensionConnectionPt(GridCompConnectionPt(v_pt), label=label)
      
      _UNUSED_DUMMY(unusable)
   end function new_ExtensionPt_from_v_pt


   logical function less_than(lhs, rhs)
      type(ExtensionConnectionPt), intent(in) :: lhs
      type(ExtensionConnectionPt), intent(in) :: rhs

      less_than = lhs%GridCompConnectionPt < rhs%GridCompConnectionPt
      if (less_than) return

      ! if greater:
      if (rhs%GridCompConnectionPt < lhs%GridCompConnectionPt) return
      less_than = lhs%label < rhs%label

   end function less_than

   logical function equal_to(lhs, rhs)
      type(ExtensionConnectionPt), intent(in) :: lhs
      type(ExtensionConnectionPt), intent(in) :: rhs

      equal_to = .not. ((lhs < rhs) .or. (rhs < lhs))
      
   end function equal_to

end module mapl3g_ExtensionConnectionPt
