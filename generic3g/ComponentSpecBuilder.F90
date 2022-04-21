#include "MAPL_ErrLog.h"

module mapl3g_ComponentSpecBuilder
   use yaFyaml, only: Configuration
   use mapl_ErrorHandling
   implicit none
   private

   public :: build_component_spec
   
contains

   type(ComponentSpec) function build_component_spec(config, rc)
      type(Configuration), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      component_spec%setservices_spec = process_setservices_spec(config%of('setservices'), _RC)
      component_spec%states_spec = process_states_spec(config%of('states'), _RC)
      component_spec%connections_spec = process_connections_spec(config%of('connections'), _RC)
      component_spec%children_spec = process_children_spec(config%of('children'), _RC)
      component_spec%grid_spec = process_grid_spec(config%of('grid', _RC)
      component_spec%services_spec = process_grid_spec(config%of('serviceservices', _RC)

      _RETURN(_SUCCESS)
   end function build_component_spec


   type(SetServicesSpec) function build_setservices_Spec(config, rc)
      type(Configuration), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      
      _RETURN(_SUCCESS)
   end function build_setservices_Spec

   type(StatesSpec) function build_states_spec(config, rc) result(states_spec)
      type(Configuration), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      states_spec%import_spec = build_state_spec(config%of('import'), _RC)
      states_spec%export_spec = build_state_spec(config%of('export'), _RC)
      states_spec%internal_spec = build_state_spec(config%of('internal'), _RC)
      
      _RETURN(_SUCCESS)
   end function build_states_spec

   type(StatesSpec) function build_state_spec(config, rc) result(state_spec)
      type(Configuration), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      state_spec%field_specs = build_var_specs(config%of('fields'), _RC)
      state_spec%bundle_specs = build_var_specs(config%of('bundles'), _RC)
      state_spec%services_spec = build_services_spec(config%of('services'), _RC)
      
      _RETURN(_SUCCESS)
   end function build_state_spec

   type(ChildrenSpec) function build_state_spec(config, rc) result(children_spec)
      type(Configuration), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status


      ...
      _RETURN(_SUCCESS)
   end function build_state_spec
      

end module mapl3g_ComponentSpecBuilder
