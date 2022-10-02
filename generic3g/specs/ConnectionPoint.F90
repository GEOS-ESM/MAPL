module mapl3g_ConnectionPoint
   use mapl3g_RelativeConnectionPoint
   implicit none
   private

   public :: ConnectionPoint
   public :: operator(<)
   public :: operator(==)

   type :: ConnectionPoint
      character(:), allocatable :: component_name
      character(:), allocatable :: state_intent
      type(RelativeConnectionPoint) :: relative_pt
   contains
!!$      procedure :: component
!!$      procedure :: state_intent
      procedure :: short_name
!!$
!!$      procedure :: is_simple
!!$      procedure :: extend
      
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


   function new_connection_point_basic(component_name, state_intent, relative_pt) result(conn_pt)
      type(ConnectionPoint) :: conn_pt
      character(*), intent(in) :: component_name
      character(*), intent(in) :: state_intent
      type(RelativeConnectionPoint), intent(in) :: relative_pt

      conn_pt%component_name = component_name
      conn_pt%state_intent = state_intent
      conn_pt%relative_pt = relative_pt
      
   end function new_connection_point_basic

   function new_connection_point_simple(component_name, state_intent, short_name) result(conn_pt)
      type(ConnectionPoint) :: conn_pt
      character(*), intent(in) :: component_name
      character(*), intent(in) :: state_intent
      character(*), intent(in) :: short_name

      conn_pt%component_name = component_name
      conn_pt%state_intent = state_intent
      conn_pt%relative_pt = RelativeConnectionPoint(short_name)
      
   end function new_connection_point_simple

   function short_name(this)
      character(:), pointer :: short_name
      class(ConnectionPoint), intent(in) :: this
      short_name => this%relative_pt%short_name()
   end function short_name

   ! We need an ordering on ConnectionPoint objects such that we can
   ! use them as keys in map containers.  Components are compared in
   ! order of decreasing variability for performance reasons.  E.g.,
   ! short names are all but unique and will almost always distinguish
   ! a connection point.   Whereas, state_intent has only 3 possibilites.
   
   logical function less(lhs, rhs)
      type(ConnectionPoint), intent(in) :: lhs, rhs

      less = (.not. (rhs%relative_pt < lhs%relative_pt))
      if (.not. less) return

      less = (lhs%component_name <= rhs%component_name)
      if (.not. less) return

      less = (lhs%state_intent < rhs%state_intent)
      
   end function less

   logical function equal_to(lhs, rhs)
      type(ConnectionPoint), intent(in) :: lhs, rhs

      equal_to = (.not. (rhs%relative_pt < lhs%relative_pt) .and. (.not. (lhs%relative_pt < rhs%relative_pt)))
      if (.not. equal_to) return

      equal_to = (lhs%component_name == rhs%component_name)
      if (.not. equal_to) return

      equal_to = (lhs%state_intent == rhs%state_intent)
      
   end function equal_to


   pure logical function is_internal(this)
      class(ConnectionPoint), intent(in) :: this
      is_internal = (this%state_intent == 'internal')
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
