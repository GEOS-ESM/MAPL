module mapl_CompositeComponent
   use mapl_AbstractFrameworkComponent
   use mapl_AbstractComposite
   use mapl_ConcreteComposite
   implicit none
   private

   public :: CompositeComponent

   type, abstract, extends(AbstractFrameworkComponent) :: CompositeComponent
!!$      private
      class(ConcreteComposite), pointer :: composite => null()
   contains
      procedure :: get_child
      procedure :: add_child
      procedure :: get_parent
      procedure :: get_num_children

      ! Indirect design pattern accessors
      procedure :: set_composite
      procedure :: get_composite
      
   end type CompositeComponent

contains

   function get_child(this, name) result(child)
      class(AbstractFrameworkComponent), pointer :: child
      class(CompositeComponent), intent(in) :: this
      character(*), intent(in) :: name

      class(AbstractComposite), pointer :: child_node

      child_node => this%composite%get_child(name)
      select type (child_node)
      class is (ConcreteComposite)
         child => child_node%get_component()
      end select
      
   end function get_child


   function add_child(this, name, component) result(child)
      class(AbstractFrameworkComponent), pointer :: child
      class(CompositeComponent), target, intent(inout) :: this
      character(*), intent(in) :: name
      class(AbstractFrameworkComponent), intent(in) :: component

      class(AbstractComposite), pointer :: child_composite

      child_composite => this%composite%add_child(name, ConcreteComposite(component, parent=this%composite))

      ! Possibly the introduction of a SurrogateComposite class at the
      ! top of the inheritance hierarchy would eliminate some of the
      ! SELECT TYPE statements below.  As an isolated instance
      ! the current solution is not completely abhorrent.

      select type (child_composite)
      class is (ConcreteComposite)
         child => child_composite%get_component()

         select type(child)
         class is (CompositeComponent)
            child%composite => child_composite
         end select
      end select

   end function add_child


   function get_parent(this) result(parent)
      class(AbstractFrameworkComponent), pointer :: parent
      class(CompositeComponent), intent(in) :: this

      class(AbstractComposite), pointer :: parent_node

      parent_node => this%composite%get_parent()
      select type (parent_node)
      class is (ConcreteComposite)
         parent => parent_node%get_component()
      end select

   end function get_parent

   integer function get_num_children(this) result(num_children)
      class(CompositeComponent), intent(in) :: this

      num_children = this%composite%get_num_children()
   end function get_num_children
      
   subroutine set_composite(this, composite)
      class(CompositeComponent), intent(inout) :: this
      class(AbstractComposite), target, intent(in) :: composite

      select type (composite)
      type is (ConcreteComposite)
         this%composite => composite
      end select
   end subroutine set_composite

   function get_composite(this) result(composite)
      class(CompositeComponent), target, intent(in) :: this
      class(AbstractComposite), pointer :: composite

      composite => this%composite
   end function get_composite

end module mapl_CompositeComponent
