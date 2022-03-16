#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) OuterMetaComponent_setservices_smod
   use esmf, only: ESMF_GridCompSetEntryPoint
   use esmf, only: ESMF_Method_Flag
   use gFTL2_StringVector
   use mapl3g_ESMF_Interfaces, only: I_Run
   ! Kludge to work around Intel 2021 namespace bug that exposes
   ! private names from other modules in unrelated submodules.
   ! Report filed 2022-03-14 (T. Clune)
   use mapl_keywordenforcer, only: KE => KeywordEnforcer
   implicit none

contains

   module subroutine SetServices(this, rc)
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
!!$      
!!$      call before(this, _RC)
!!$
!!$      if (this%has_yaml_config()) then
!!$         associate(config => this%get_yaml_config())
!!$           call this%set_component_spec(build_component_spec(config, _RC))
!!$         end associate
!!$      end if
!!$
!!$      
!!$      user_gc = create_user_gridcomp(this, _RC)
!!$      call this%run_user_setservices(user_gc, _RC)
!!$
!!$      call set_outer_gc_entry_points(this, _RC)
!!$
!!$      call <messy stuff>
!!$
!!$      ...
      
      _RETURN(ESMF_SUCCESS)
   end subroutine SetServices


   subroutine set_entry_point(this, method_flag, userProcedure, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Method_Flag), intent(in) :: method_flag
      procedure(I_Run) :: userProcedure
      class(KE), optional, intent(in) :: unusable
      character(len=*), intent(in) :: phase_name
      integer, optional, intent(out) ::rc

      integer :: status

      call add_phase(this%phases_map, method_flag=method_flag, phase_name=phase_name, _RC)

      associate(phase_idx => get_phase_index(this%phases_map%of(method_flag), phase_name))
        call ESMF_GridCompSetEntryPoint(this%user_gc, method_flag, userProcedure, phase=phase_idx, _RC)
      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine set_entry_point


   ! This should move to a separate module.
!!$   function build_component_spec(config, rc) result(component_spec)
!!$      type(ComponentSpec) :: component_spec
!!$
!!$      component_spec%setservices_spec = process_setservices_spec(config%of('setservices'), _RC)
!!$      component_spec%states_spec = process_states_spec(config%of('states'), _RC)
!!$      component_spec%connections_spec = process_connections_spec(config%of('connections'), _RC)
!!$      component_spec%children_spec = process_children_spec(config%of('children'), _RC)
!!$      component_spec%grid_spec = process_grid_spec(config%of('grid', _RC)
!!$      component_spec%services_spec = process_grid_spec(config%of('serviceservices', _RC)
!!$
!!$      _RETURN(_SUCCESS)
!!$   end function build_component_spec

end submodule OuterMetaComponent_setservices_smod
