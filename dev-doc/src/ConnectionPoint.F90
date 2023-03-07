module oomph_ConnectionPoint
   implicit none
   private

   public :: ConnectionPoint

   type :: ConnectionPoint
      character(:), allocatable :: component
      character(:), allocatable :: state_item
   end type ConnectionPoint

   interface ConnectionPoint
      module procedure :: new_ConnectionPoint
   end interface ConnectionPoint

contains

   function new_ConnectionPoint(component, state_item) result(connection_point)
      type(ConnectionPoint) :: connection_point
      character(*), intent(in) :: component
      character(*), intent(in) :: state_item

      connection_point%component = component
      connection_point%state_item = state_item

   end function new_ConnectionPoint

      
end module oomph_ConnectionPoint
