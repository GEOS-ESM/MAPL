module mapl3g_RelativeConnectionPoint
   use gftl2_StringVector
   implicit none
   private
  
   public :: RelativeConnectionPoint
   public :: operator(<)
  
   type :: RelativeConnectionPoint
      type(StringVector) :: substates
   contains
      procedure :: short_name
   end type RelativeConnectionPoint
  
   interface operator(<)
      module procedure less
   end interface operator(<)

   interface RelativeConnectionPoint
      module procedure new_relconpt_one
      module procedure new_relconpt_arr
      module procedure new_relconpt_vec
   end interface RelativeConnectionPoint
   
contains

   function new_relconpt_one(short_name) result(conn_pt)
      type(RelativeConnectionPoint) :: conn_pt
      character(*), intent(in) :: short_name
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

   function short_name(this)
      character(:), pointer :: short_name
      class(RelativeConnectionPoint), target, intent(in) :: this
      short_name => this%substates%back()
   end function short_name

   logical function less(lhs, rhs)
      type(RelativeConnectionPoint), intent(in) :: lhs
      type(RelativeConnectionPoint), intent(in) :: rhs
      less = lhs%substates < rhs%substates
   end function less

end module mapl3g_RelativeConnectionPoint
