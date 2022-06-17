module mapl3g_AbstractStateItemSpec
   implicit none
   private

   public :: AbstractStateItemSpec

   type, abstract :: AbstractStateItemSpec
      private
      character(:), allocatable :: name
   contains
      procedure, non_overridable :: set_name
      procedure, non_overridable :: get_name
   end type AbstractStateItemSpec

contains


   pure subroutine set_name(this, name)
      class(AbstractStateItemSpec), intent(inout) :: this
      character(*), intent(in) :: name
      this%name = name
   end subroutine set_name
   

   pure function get_name(this) result(name)
      character(:), allocatable :: name
      class(AbstractStateItemSpec), intent(in) :: this
      name = this%name
   end function get_name
   

end module mapl3g_AbstractStateItemSpec
