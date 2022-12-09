#include "MAPL_Generic.h"

module mapl3g_ExtensionConnectionPt
   use mapl3g_newVirtualConnectionPt
   use mapl3g_newActualConnectionPt
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private
  
   public :: ExtensionConnectionPt
   public :: operator(<)
   public :: operator(==)

   type, extends(newActualConnectionPt) :: ExtensionConnectionPt
      private
      integer :: label = 0
   contains
      procedure :: increment
      procedure :: get_esmf_name
   end type ExtensionConnectionPt

   ! Constructors
   interface ExtensionConnectionPt
      module procedure new_ExtensionPt_from_gc_pt
      module procedure new_ExtensionPt_from_v_pt
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
      type(newActualConnectionPt), intent(in) :: gc_pt
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: label

      ext_pt%newActualConnectionPt = gc_pt
      if (present(label)) ext_pt%label = label
      
      _UNUSED_DUMMY(unusable)
   end function new_ExtensionPt_from_gc_pt


   function new_ExtensionPt_from_v_pt(v_pt, unusable, label) result(ext_pt)
      type(ExtensionConnectionPt) :: ext_pt
      type(newVirtualConnectionPt), intent(in) :: v_pt
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: label

      ext_pt = ExtensionConnectionPt(newActualConnectionPt(v_pt), label=label)
      
      _UNUSED_DUMMY(unusable)
   end function new_ExtensionPt_from_v_pt

   ! Usually we just want to just increment the label when we encounter
   ! the need for a new extension point.
   function increment(this) result(new_pt)
      type(ExtensionConnectionPt) :: new_pt
      class(ExtensionConnectionPt), intent(in) :: this

      new_pt = this
      new_pt%label = new_pt%label + 1
      
   end function increment

   ! Important that name is different if either comp_name or short_name differ
   function get_esmf_name(this) result(name)
      character(:), allocatable :: name
      class(ExtensionConnectionPt), intent(in) :: this

      character(16) :: buf

      write(buf, '(i0)') this%label
      name = this%newActualConnectionPt%get_esmf_name() // '(' // trim(buf) // ')'

   end function get_esmf_name

   logical function less_than(lhs, rhs)
      type(ExtensionConnectionPt), intent(in) :: lhs
      type(ExtensionConnectionPt), intent(in) :: rhs

      less_than = lhs%newActualConnectionPt < rhs%newActualConnectionPt
      if (less_than) return

      ! if greater:
      if (rhs%newActualConnectionPt < lhs%newActualConnectionPt) return

      ! Tie breaker
      less_than = lhs%label < rhs%label

   end function less_than

   logical function equal_to(lhs, rhs)
      type(ExtensionConnectionPt), intent(in) :: lhs
      type(ExtensionConnectionPt), intent(in) :: rhs

      equal_to = .not. ((lhs < rhs) .or. (rhs < lhs))
      
   end function equal_to

end module mapl3g_ExtensionConnectionPt
