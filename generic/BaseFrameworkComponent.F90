module mapl_BaseFrameworkComponent
   use ESMF
   use mapl_SurrogateFrameworkComponent
   use mapl_AbstractFrameworkComponent
   use mapl_CompositeComponent
   use mapl_AbstractComponent
   use mapl_ComponentSpecification
   use mapl_MaplGrid
   implicit none
   private

   public :: BaseFrameworkComponent

   type, abstract, extends(CompositeComponent) :: BaseFrameworkComponent
      ! private
      class(AbstractComponent), allocatable :: component
      type(ComponentSpecification) :: component_spec
      type(MaplGrid) :: grid
      ! type(PhaseMap) :: run_phase_map
   contains
      procedure :: set_component
      procedure :: get_component
      procedure :: get_internal_state
   end type BaseFrameworkComponent

contains

   subroutine set_component(this, component)
      class(BaseFrameworkComponent), target, intent(inout) :: this
      class(AbstractComponent), intent(in) :: component

      this%component = component
      call this%component%set_framework(this)

   end subroutine set_component


   function get_component(this) result(component)
      class(AbstractComponent), pointer :: component
      class(BaseFrameworkComponent), target, intent(in) :: this

      component => this%component
   end function get_component

   
   function get_internal_state(this) result(state)
      type(ESMF_State), pointer :: state
      class(BaseFrameworkComponent), target, intent(in) :: this

      state => this%component%get_internal_state()
   end function get_internal_state

end module mapl_BaseFrameworkComponent
