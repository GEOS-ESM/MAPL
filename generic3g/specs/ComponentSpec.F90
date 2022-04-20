module mapl3g_ComponentSpec
   use mapl3g_UserSetServices
   implicit none
   private

   public :: ComponentSpec

   type :: ComponentSpec
      class(AbstractUserSetServices), allocatable :: user_setservices
!!$      type(StatesSpec) :: states_spec
!!$      type(ChildrenSpecMap) :: child_specs
   end type ComponentSpec

   interface ComponentSpec
      module procedure new_ComponentSpec
   end interface ComponentSpec

contains

   function new_ComponentSpec() result(spec)
      type(ComponentSpec) :: spec
   end function new_ComponentSpec

!!$   function new_ComponentSpec(states_spec, child_specs) result(spec)
!!$      type(ComponentSpec) :: spec
!!$      type(StatesSpec), intent(in) :: states_spec
!!$      type(ChildSpecMap), intent(in) :: child_specs
!!$
!!$      spec%states_spec = states_spec
!!$      spec%child_specs = child_specs
!!$      
!!$   end function new_ComponentSpec
      

end module mapl3g_ComponentSpec
