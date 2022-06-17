module mapl3g_StateSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecMap
   implicit none
   private

   public :: StateSpec
   type, extends(AbstractStateItemSpec) :: StateSpec
      private
      type(StateItemSpecMap) :: items
   contains
      procedure :: add_item
      procedure :: get_item
   end type StateSpec

contains

   subroutine add_item(this, name, item)
      class(StateSpec), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      class(AbstractStateItemSpec), intent(in) :: item

      call this%items%insert(name, item)

   end subroutine add_item

   function get_item(this, name) result(item)
      class(AbstractStateItemSpec), pointer :: item
      class(StateSpec), target, intent(inout) :: this
      character(len=*), intent(in) :: name

      integer :: status

      item => this%items%at(name, rc=status)

   end function get_item

end module mapl3g_StateSpec
