module mapl3g_RelativeConnectionPoint
   use gftl2_StringVector
   implicit none
   private
  
   public :: RelativeConnectionPoint
   public :: operator(<)
   public :: operator(==)
  
   type :: RelativeConnectionPoint
      type(StringVector) :: substates
   contains
      procedure :: short_name
      procedure :: state_intent
      procedure :: is_import
      procedure :: is_internal
   end type RelativeConnectionPoint
  
   interface operator(<)
      module procedure less
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface RelativeConnectionPoint
      module procedure new_relconpt_one
      module procedure new_relconpt_arr
      module procedure new_relconpt_vec
   end interface RelativeConnectionPoint
   
contains

   function new_relconpt_one(state_intent, short_name) result(conn_pt)
      type(RelativeConnectionPoint) :: conn_pt
      character(*), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      call conn_pt%substates%push_back(state_intent)
      call conn_pt%substates%push_back(short_name)
   end function new_relconpt_one
   
   function new_relconpt_arr(list) result(conn_pt)
      type(RelativeConnectionPoint) :: conn_pt
      character(*), intent(in) :: list(:)

      integer :: i

      do i = 1, size(list)
         call conn_pt%substates%push_back(list(i))
      end do

   end function new_relconpt_arr

   function new_relconpt_vec(vec) result(conn_pt)
      type(RelativeConnectionPoint) :: conn_pt
      type(StringVector), intent(in) :: vec

      conn_pt%substates = vec

   end function new_relconpt_vec

   ! Short name is always the last item in the nesting.
   function short_name(this)
      character(:), pointer :: short_name
      class(RelativeConnectionPoint), target, intent(in) :: this
      short_name => this%substates%back()
   end function short_name

   ! state intent is always the top item in nestingn
   function state_intent(this)
      character(:), pointer :: state_intent
      class(RelativeConnectionPoint), target, intent(in) :: this
      state_intent => this%substates%front()
   end function state_intent

   logical function less(lhs, rhs)
      type(RelativeConnectionPoint), intent(in) :: lhs
      type(RelativeConnectionPoint), intent(in) :: rhs
      less = lhs%substates < rhs%substates
   end function less

   logical function equal_to(lhs, rhs)
      type(RelativeConnectionPoint), intent(in) :: lhs
      type(RelativeConnectionPoint), intent(in) :: rhs
      equal_to = lhs%substates == rhs%substates
   end function equal_to

   logical function is_import(this)
      class(RelativeConnectionPoint), intent(in) :: this
      is_import = (this%state_intent() == 'import')
   end function is_import

   logical function is_internal(this)
      class(RelativeConnectionPoint), intent(in) :: this
      is_internal = (this%state_intent() == 'internal')
   end function is_internal

end module mapl3g_RelativeConnectionPoint
