module mapl_ConcreteComposite
   use mapl_AbstractFrameworkComponent
   use mapl_AbstractComposite
   use mapl_StringCompositeMap
   implicit none
   private

   public :: ConcreteComposite

   type, extends(AbstractComposite) :: ConcreteComposite
      private
      class(AbstractFrameworkComponent), allocatable :: component
      type(StringCompositeMap) :: children
      class(ConcreteComposite), pointer :: parent => null()
   contains
      procedure :: add_child
      procedure :: get_child
      procedure :: get_parent
      procedure :: get_component
      procedure :: set_component
      procedure :: get_num_children
      procedure :: initialize
!!$      procedure :: is_leaf
!!$      procedure :: is_root
   end type ConcreteComposite

   interface ConcreteComposite
      module procedure new_root_composite
      module procedure new_composite
      module procedure new_placeholder
   end interface ConcreteComposite

contains

   function new_root_composite(component) result(composite)
      type(ConcreteComposite) :: composite
      class(AbstractFrameworkComponent), intent(in) :: component
      composite%component = component
      composite%parent => null()
   end function new_root_composite

   function new_composite(component, parent) result(composite)
      type(ConcreteComposite) :: composite
      class(AbstractFrameworkComponent), intent(in) :: component
      class(ConcreteComposite), target, intent(in) :: parent

      composite%component = component
      composite%parent => parent

   end function new_composite

   ! GFortran 10.1 crashes when the constructor (function) is used
   ! probably because of the deep copy in the instrinsic assignment.
   ! this initialize() method is a workaround.
   subroutine initialize(this, component)
      class(ConcreteComposite), intent(inout) :: this
      class(AbstractFrameworkComponent), intent(in) :: component
      this%component = component
      this%parent => null()
   end subroutine initialize

   function new_placeholder(parent) result(composite)
      type(ConcreteComposite) :: composite
      class(ConcreteComposite), target, intent(in) :: parent

      composite%parent => parent

   end function new_placeholder

   function get_child(this, name) result(child)
      class(AbstractComposite), pointer :: child
      class(ConcreteComposite), intent(in) :: this
      character(*), intent(in) :: name

      child => this%children%at(name)

   end function get_child


   function add_child(this, name, composite) result(child)
      class(AbstractComposite), pointer :: child
      class(ConcreteComposite), target, intent(inout) :: this
      character(*), intent(in) :: name
      class(AbstractComposite), intent(in) :: composite

      call this%children%insert(name, composite)
      child => this%children%at(name)

   end function add_child


   function get_parent(this) result(parent)
      class(AbstractComposite), pointer :: parent
      class(ConcreteComposite), intent(in) :: this

      parent => this%parent

   end function get_parent

   function get_component(this) result(component)
      class(AbstractFrameworkComponent), pointer :: component
      class(ConcreteComposite), target, intent(in) :: this

      component => this%component
   end function get_component

   subroutine set_component(this, component)
      class(ConcreteComposite), intent(inout) :: this
      class(AbstractFrameworkComponent), intent(in) :: component

      this%component = component

   end subroutine set_component

   integer function get_num_children(this) result(num_children)
      class(ConcreteComposite), intent(in) :: this

      num_children = this%children%size()
   end function get_num_children
      
end module mapl_ConcreteComposite
