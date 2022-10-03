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


contains

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
