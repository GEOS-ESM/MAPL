module mapl3g_ConnectionPoint
   use mapl3g_RelativeConnectionPoint
   implicit none
   private

   public :: ConnectionPoint
   public :: operator(<)
   public :: operator(==)

   type :: ConnectionPoint
      character(:), allocatable :: component_name
      type(RelativeConnectionPoint) :: relative_pt
   contains
      procedure :: is_import
      procedure :: is_internal
      procedure :: short_name
      procedure :: state_intent
   end type ConnectionPoint

   interface operator(<)
      module procedure less
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface ConnectionPoint
      module procedure new_connection_point_basic
      module procedure new_connection_point_simple
   end interface ConnectionPoint

contains


   function new_connection_point_basic(component_name, relative_pt) result(conn_pt)
      type(ConnectionPoint) :: conn_pt
      character(*), intent(in) :: component_name
      type(RelativeConnectionPoint), intent(in) :: relative_pt

      conn_pt%component_name = component_name
      conn_pt%relative_pt = relative_pt
      
   end function new_connection_point_basic

   function new_connection_point_simple(component_name, state_intent, short_name) result(conn_pt)
      type(ConnectionPoint) :: conn_pt
      character(*), intent(in) :: component_name
      character(*), intent(in) :: state_intent
      character(*), intent(in) :: short_name

      conn_pt%component_name = component_name
      conn_pt%relative_pt = RelativeConnectionPoint(state_intent, short_name)
      
   end function new_connection_point_simple

   function short_name(this)
      character(:), pointer :: short_name
      class(ConnectionPoint), intent(in) :: this
      short_name => this%relative_pt%short_name()
   end function short_name

   function state_intent(this)
      character(:), pointer :: state_intent
      class(ConnectionPoint), intent(in) :: this
      state_intent => this%relative_pt%state_intent()
   end function state_intent

   ! We need an ordering on ConnectionPoint objects such that we can
   ! use them as keys in map containers.  Components are compared in
   ! order of decreasing variability for performance reasons.  E.g.,
   ! short names are all but unique and will almost always distinguish
   ! a connection point.   Whereas, state_intent has only 3 possibilites.
   
   logical function less(lhs, rhs)
      type(ConnectionPoint), intent(in) :: lhs, rhs

      logical :: greater

      less = (lhs%component_name < rhs%component_name)
      if (less) return
      greater = (rhs%component_name < lhs%component_name)
      if (greater) return
      
      ! tie so far
      less = (lhs%relative_pt < rhs%relative_pt)

   end function less

   logical function equal_to(lhs, rhs)
      type(ConnectionPoint), intent(in) :: lhs, rhs

      equal_to = (lhs%relative_pt == rhs%relative_pt)
      if (.not. equal_to) return

      equal_to = (lhs%component_name == rhs%component_name)
      if (.not. equal_to) return

   end function equal_to


   logical function is_import(this)
      class(ConnectionPoint), intent(in) :: this
      is_import = (this%state_intent() == 'import')
   end function is_import

   logical function is_internal(this)
      class(ConnectionPoint), intent(in) :: this
      is_internal = (this%state_intent() == 'internal')
   end function is_internal


!!$   function extend(this) result(extension_pt, ith)
!!$      type(ConnectionPoint) :: extension_pt
!!$      class(ConnectionPoint), intent(in) :: this
!!$      integer, intent(in) :: ith
!!$
!!$      extension_pt = this
!!$      call extension_pt%nesting%pop_back()
!!$      associate (short_name => this%short_name())
!!$        call extension_pt%push_back('extension(' // short_name // ')')
!!$        call extension_pt%push_back(short_name // '(' // to_string(ith) // ')')
!!$      end associate
!!$   end function extend
   

end module mapl3g_ConnectionPoint
