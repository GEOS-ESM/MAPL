module mapl_BaseComponent
   use mapl_AbstractComponent
   use pFlogger, only: fLogger => Logger
   implicit none
   private

   public :: BaseComponent

   type, abstract, extends(AbstractComponent) :: BaseComponent
      private
      character(:), allocatable :: name
   contains
      procedure :: set_name
      procedure :: get_name
   end type BaseComponent

contains


   subroutine set_name(this, name)
      class(BaseComponent), intent(inout) :: this
      character(*), intent(in) :: name

      this%name = name

   end subroutine set_name


   function get_name(this) result(name)
      character(:), allocatable :: name
      class(BaseComponent), intent(in) :: this

      name = this%name

   end function get_name

      
end module mapl_BaseComponent
