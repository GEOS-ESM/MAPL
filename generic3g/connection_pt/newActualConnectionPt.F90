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
   contains
      procedure :: get_state_intent
      procedure :: get_esmf_name
      procedure :: add_comp_name
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

   function new_newActualPt_from_v_pt(v_pt) result(a_pt)
      type(newActualConnectionPt) :: a_pt
      type(newVirtualConnectionPt), intent(in) :: v_pt

      a_pt%v_pt = v_pt

   end function new_newActualPt_from_v_pt


   function add_comp_name(this, comp_name) result(a_pt)
      type(newActualConnectionPt) :: a_pt
      class(newActualConnectionPt), intent(in) :: this
      character(*), intent(in) :: comp_name

      a_pt%v_pt = this%v_pt%add_comp_name(comp_name)
      
   end function add_comp_name


   function get_state_intent(this) result(state_intent)
      character(:), allocatable :: state_intent
      class(newActualConnectionPt), intent(in) :: this

      state_intent = this%v_pt%get_state_intent()

   end function get_state_intent


   ! Important that name is different if either comp_name or short_name differ
   function get_esmf_name(this) result(name)
      character(:), allocatable :: name
      class(newActualConnectionPt), intent(in) :: this

      name = this%v_pt%get_esmf_name()
         
   end function get_esmf_name


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
