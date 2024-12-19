module MAPL_SplitCommunicatorMod
   implicit none
   private

   public :: SplitCommunicator
   public :: NULL_SUBCOMMUNICATOR_NAME

   type :: SplitCommunicator
      private
      integer :: subcommunicator
      integer :: color
      character(:), allocatable :: name
   contains
      procedure :: get_subcommunicator
      procedure :: get_color
      procedure :: get_name
   end type SplitCommunicator

   character(*), parameter :: NULL_SUBCOMMUNICATOR_NAME = 'NULL'

   interface SplitCommunicator
      module procedure new_SplitCommunicator
   end interface SplitCommunicator


contains


   function new_SplitCommunicator(subcommunicator, color, name) result(split)
      type (SplitCommunicator) :: split
      integer, intent(in) :: subcommunicator
      integer, intent(in) :: color
      character(*), intent(in) :: name

      split%subcommunicator = subcommunicator
      split%color = color
      split%name = name

   end function new_SplitCommunicator

   integer function get_subcommunicator(this) result(subcommunicator)
      class (SplitCommunicator), intent(in) :: this
      subcommunicator = this%subcommunicator
   end function get_subcommunicator
   
   integer function get_color(this) result(color)
      class (SplitCommunicator), intent(in) :: this
      color = this%color
   end function get_color
   
   function get_name(this) result(name)
      character(:), allocatable :: name
      class (SplitCommunicator), intent(in) :: this
      name = this%name
   end function get_name

   
end module MAPL_SplitCommunicatorMod


