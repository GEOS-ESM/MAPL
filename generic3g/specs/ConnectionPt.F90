module mapl3g_ConnectionPt
   use mapl3g_VirtualConnectionPt
   implicit none
   private

   public :: ConnectionPt
   public :: operator(<)
   public :: operator(==)

   type :: ConnectionPt
      character(:), allocatable :: component_name
      type(VirtualConnectionPt) :: virtual_pt
   contains
      procedure :: is_import
      procedure :: is_export
      procedure :: is_internal
      procedure :: short_name
      procedure :: state_intent
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


   function new_connection_point_basic(component_name, virtual_pt) result(conn_pt)
      type(ConnectionPt) :: conn_pt
      character(*), intent(in) :: component_name
      type(VirtualConnectionPt), intent(in) :: virtual_pt

      conn_pt%component_name = component_name
      conn_pt%virtual_pt = virtual_pt
      
   end function new_connection_point_basic

   function new_connection_point_simple(component_name, state_intent, short_name) result(conn_pt)
      type(ConnectionPt) :: conn_pt
      character(*), intent(in) :: component_name
      character(*), intent(in) :: state_intent
      character(*), intent(in) :: short_name

      conn_pt%component_name = component_name
      conn_pt%virtual_pt = VirtualConnectionPt(state_intent, short_name)
      
   end function new_connection_point_simple

   function short_name(this)
      character(:), pointer :: short_name
      class(ConnectionPt), intent(in) :: this
      short_name => this%virtual_pt%short_name()
   end function short_name

   function state_intent(this)
      character(:), pointer :: state_intent
      class(ConnectionPt), intent(in) :: this
      state_intent => this%virtual_pt%state_intent()
   end function state_intent

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
      less = (lhs%virtual_pt < rhs%virtual_pt)

   end function less

   logical function equal_to(lhs, rhs)
      type(ConnectionPt), intent(in) :: lhs, rhs

      equal_to = (lhs%virtual_pt == rhs%virtual_pt)
      if (.not. equal_to) return

      equal_to = (lhs%component_name == rhs%component_name)
      if (.not. equal_to) return

   end function equal_to


   logical function is_import(this)
      class(ConnectionPt), intent(in) :: this
      is_import = (this%state_intent() == 'import')
   end function is_import

   logical function is_export(this)
      class(ConnectionPt), intent(in) :: this
      is_export = (this%state_intent() == 'export')
   end function is_export

   logical function is_internal(this)
      class(ConnectionPt), intent(in) :: this
      is_internal = (this%state_intent() == 'internal')
   end function is_internal


!!$   function extend(this) result(extension_pt, ith)
!!$      type(ConnectionPt) :: extension_pt
!!$      class(ConnectionPt), intent(in) :: this
!!$      integer, intent(in) :: ith
!!$
!!$      extension_pt = this
!!$      call extension_pt%nesting%pop_back()
!!$      associate (short_name => this%short_name())
!!$        call extension_pt%push_back('extension(' // short_name // ')')
!!$        call extension_pt%push_back(short_name // '(' // to_string(ith) // ')')
!!$      end associate
!!$   end function extend
   

end module mapl3g_ConnectionPt
