module mapl_AbstractFrameworkComponent
   use mapl_SurrogateFrameworkComponent
   implicit none
   private

   public :: AbstractFrameworkComponent

   type, abstract, extends(SurrogateFrameworkComponent) :: AbstractFrameworkComponent
   contains
      procedure(i_AddChildComponent), deferred :: add_child_component
      ! accessors
      procedure(i_SetComponent), deferred :: set_component
      procedure(i_GetComponent), deferred :: get_component
      procedure(i_SetComposite), deferred :: set_composite
      procedure(i_GetState), deferred :: get_internal_state
   end type AbstractFrameworkComponent

   abstract interface

      function i_AddChildComponent(this, name, user_component) result(child)
         use mapl_AbstractComponent
         import AbstractFrameworkComponent
         class(AbstractFrameworkComponent), pointer :: child
         class(AbstractFrameworkComponent), target, intent(inout) :: this
         character(*), intent(in) :: name
         class(AbstractComponent), intent(in) :: user_component
      end function i_AddChildComponent

      subroutine i_SetComponent(this, component)
         use mapl_AbstractComponent
         import AbstractFrameworkComponent
         class(AbstractFrameworkComponent), target, intent(inout) :: this
         class(AbstractComponent), intent(in) :: component
      end subroutine i_SetComponent


      function i_GetComponent(this) result(component)
         use mapl_AbstractComponent
         import AbstractFrameworkComponent
         class(AbstractComponent), pointer :: component
         class(AbstractFrameworkComponent), target, intent(in) :: this
      end function i_GetComponent

      subroutine i_SetComposite(this, composite)
         use mapl_AbstractComposite
         import AbstractFrameworkComponent
         implicit none
         class(AbstractFrameworkComponent), intent(inout) :: this
         class(AbstractComposite), target, intent(in) :: composite
      end subroutine i_SetComposite

      function i_GetState(this) result(state)
         use ESMF
         import AbstractFrameworkComponent
         type(ESMF_State), pointer :: state
         class(AbstractFrameworkComponent), target, intent(in) :: this
      end function i_GetState
   end interface

end module mapl_AbstractFrameworkComponent
