module mapl_AbstractComposite
   implicit none
   private

   public :: AbstractComposite

   type, abstract :: AbstractComposite
   contains
      procedure(i_GetChild), deferred :: get_child
      procedure(i_AddChild), deferred :: add_child
      procedure(i_GetParent), deferred :: get_parent
      procedure(i_GetNum), deferred :: get_num_children
!!$      procedure :: is_leaf
!!$      procedure :: is_root
!!$      procedure :: get_height
   end type AbstractComposite

   abstract interface

      function i_GetChild(this, name) result(child)
         import AbstractComposite
         class(AbstractComposite), pointer :: child
         class(AbstractComposite), intent(in) :: this
         character(*), intent(in) :: name
      end function i_GetChild

      function i_AddChild(this, name, composite) result(child)
         import AbstractComposite
         class(AbstractComposite), pointer :: child
         class(AbstractComposite), target, intent(inout) :: this
         character(*), intent(in) :: name
         class(AbstractComposite), intent(in) :: composite
      end function i_AddChild

      function i_GetParent(this) result(parent)
         import AbstractComposite
         class(AbstractComposite), pointer :: parent
         class(AbstractComposite), intent(in) :: this
      end function i_GetParent

      integer function i_GetNum(this) result(num)
         import AbstractComposite
         class(AbstractComposite), intent(in) :: this
      end function i_GetNum

   end interface

end module mapl_AbstractComposite
