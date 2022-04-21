#include "MAPL_ErrLog.h"

module mapl3g_ComponentSpecBuilder
   use mapl3g_ComponentSpec
   use mapl_ErrorHandling
   use mapl3g_UserSetServices
   use yaFyaml
   implicit none
   private

   ! 
   public :: build_component_spec

   ! The following interfaces are public only for testing purposes.
   public :: build_setservices
   
contains

   type(ComponentSpec) function build_component_spec(config, rc) result(spec)
      class(YAML_Node), intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      ! Set services is special because "traditional" MAPL gridcomps may
      ! have set a procedure during construction of an earlier phase.
      if (config%has('setServices')) then
         _ASSERT(.not. allocated(spec%user_setservices), 'user setservices already specified')
         spec%user_setservices = build_setservices(config%of('setServices'), _RC)
      end if
      
!!$      spec%states_spec = process_states_spec(config%of('states'), _RC)
!!$      spec%connections_spec = process_connections_spec(config%of('connections'), _RC)
!!$      spec%children_spec = process_children_spec(config%of('children'), _RC)
!!$      spec%grid_spec = process_grid_spec(config%of('grid', _RC)
!!$      spec%services_spec = process_grid_spec(config%of('serviceservices', _RC)

      _RETURN(_SUCCESS)
   end function build_component_spec


   type(DSOSetServices) function build_setservices(config, rc) result(user_ss)
      class(YAML_Node), intent(in) :: config
      integer, optional, intent(out) :: rc

      character(:), allocatable :: sharedObj, userRoutine
      integer :: status

      call config%get(sharedObj, 'sharedObj', _RC)
      if (config%has('userRoutine')) then
         call config%get(userRoutine, 'userRoutine', _RC)
      else
         userRoutine = 'setservices_'
      end if

      user_ss = user_setservices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function build_setservices

!!$   type(StatesSpec) function build_states_spec(config, rc) result(states_spec)
!!$      type(Configuration), intent(in) :: config
!!$      integer, optional, intent(out) :: rc
!!$
!!$      integer :: status
!!$
!!$      states_spec%import_spec = build_state_spec(config%of('import'), _RC)
!!$      states_spec%export_spec = build_state_spec(config%of('export'), _RC)
!!$      states_spec%internal_spec = build_state_spec(config%of('internal'), _RC)
!!$      
!!$      _RETURN(_SUCCESS)
!!$   end function build_states_spec
!!$
!!$   type(StatesSpec) function build_state_spec(config, rc) result(state_spec)
!!$      type(Configuration), intent(in) :: config
!!$      integer, optional, intent(out) :: rc
!!$
!!$      integer :: status
!!$
!!$      state_spec%field_specs = build_var_specs(config%of('fields'), _RC)
!!$      state_spec%bundle_specs = build_var_specs(config%of('bundles'), _RC)
!!$      state_spec%services_spec = build_services_spec(config%of('services'), _RC)
!!$      
!!$      _RETURN(_SUCCESS)
!!$   end function build_state_spec
!!$
!!$   type(ChildrenSpec) function build_state_spec(config, rc) result(children_spec)
!!$      type(Configuration), intent(in) :: config
!!$      integer, optional, intent(out) :: rc
!!$
!!$      integer :: status
!!$
!!$
!!$      ...
!!$      _RETURN(_SUCCESS)
!!$   end function build_state_spec
      

end module mapl3g_ComponentSpecBuilder
