module mapl3g_ConnectionPt
   use mapl3g_newVirtualConnectionPt
   implicit none
   private

   public :: ConnectionPt
   public :: operator(<)
   public :: operator(==)

   type :: ConnectionPt
      character(:), allocatable :: component_name
      type(newVirtualConnectionPt) :: v_pt
   contains
      procedure :: is_import
      procedure :: is_export
      procedure :: is_internal
      procedure :: get_esmf_name
      procedure :: get_state_intent
   end type ConnectionPt

   interface operator(<)
      module procedure less
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface ConnectionPt
      module procedure new_connection_point_basic
      module procedure new_connection_point_simple
   end interface ConnectionPt

contains


   function new_connection_point_basic(component_name, v_pt) result(conn_pt)
      type(ConnectionPt) :: conn_pt
      character(*), intent(in) :: component_name
      type(newVirtualConnectionPt), intent(in) :: v_pt

      conn_pt%component_name = component_name
      conn_pt%v_pt = v_pt
      
   end function new_connection_point_basic

   function new_connection_point_simple(component_name, state_intent, short_name) result(conn_pt)
      type(ConnectionPt) :: conn_pt
      character(*), intent(in) :: component_name
      character(*), intent(in) :: state_intent
      character(*), intent(in) :: short_name

      conn_pt%component_name = component_name
      conn_pt%v_pt = newVirtualConnectionPt(state_intent=state_intent, short_name=short_name)
      
   end function new_connection_point_simple

   function get_esmf_name(this) result(esmf_name)
      character(:), allocatable :: esmf_name
      class(ConnectionPt), intent(in) :: this
      esmf_name = this%v_pt%get_esmf_name()
   end function get_esmf_name

   function get_state_intent(this) result(state_intent)
      character(:), allocatable :: state_intent
      class(ConnectionPt), intent(in) :: this
      state_intent = this%v_pt%get_state_intent()
   end function get_state_intent

   ! We need an ordering on ConnectionPt objects such that we can
   ! use them as keys in map containers.  Components are compared in
   ! order of decreasing variability for performance reasons.  E.g.,
   ! short names are all but unique and will almost always distinguish
   ! a connection point.   Whereas, state_intent has only 3 possibilites.
   
   logical function less(lhs, rhs)
      type(ConnectionPt), intent(in) :: lhs, rhs

      logical :: greater

      less = (lhs%component_name < rhs%component_name)
      if (less) return
      greater = (rhs%component_name < lhs%component_name)
      if (greater) return
      
      ! tie so far
      less = (lhs%v_pt < rhs%v_pt)

   end function less

   logical function equal_to(lhs, rhs)
      type(ConnectionPt), intent(in) :: lhs, rhs

      equal_to = (lhs%v_pt == rhs%v_pt)
      if (.not. equal_to) return

      equal_to = (lhs%component_name == rhs%component_name)
      if (.not. equal_to) return

   end function equal_to


   logical function is_import(this)
      class(ConnectionPt), intent(in) :: this
      is_import = (this%get_state_intent() == 'import')
   end function is_import

   logical function is_export(this)
      class(ConnectionPt), intent(in) :: this
      is_export = (this%get_state_intent() == 'export')
   end function is_export

   logical function is_internal(this)
      class(ConnectionPt), intent(in) :: this
      is_internal = (this%get_state_intent() == 'internal')
   end function is_internal

end module mapl3g_ConnectionPt
