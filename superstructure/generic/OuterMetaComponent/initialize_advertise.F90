#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_advertise_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_ADVERTISE
   use mapl_VirtualConnectionPt_mod
   use mapl_StateItem_mod
   use mapl_VariableSpec_mod
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator
   use esmf, only: operator(==)
   use mapl_Connection_mod
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_VariableSpecVector_mod, only: operator(/=)
   use mapl_StateItemSpec_mod
   use mapl_MultiState_mod
   use mapl_GeometryClassAspect_mod
   use mapl_GeometrySpec_mod, only: GEOMETRY_PROVIDER, GEOMETRY_FROM_PARENT, GEOMETRY_FROM_CHILD
   use mapl_InternalConstants_mod, only: MAPL_FRAMEWORK_NAMESPACE
   use mapl_ErrorHandling_mod
   implicit none (type, external)

contains

   module recursive subroutine initialize_advertise(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(MultiState) :: user_states
      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE'

      call advertise_framework_geometry(this, _RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_ADVERTISE, _RC)
      call advertise_state_items(this, _RC)
      call run_advertise_callbacks(this, PHASE_NAME, _RC)
      call activate_connections(this, _RC)
      call this%registry%propagate_unsatisfied_imports(_RC)
      call this%registry%propagate_exports(_RC)
      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_advertise

   subroutine advertise_framework_geometry(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(GeometryCarrierLayout) :: layout
      type(StateItemSpec) :: item_spec
      type(StateItemSpec), pointer :: primary
      type(VirtualConnectionPt) :: virtual_pt
      type(ComponentSpec), pointer :: component_spec
      integer :: status

      component_spec => this%get_component_spec()
      layout = make_geometry_carrier_layout(component_spec%geometry_spec, &
           has_children=this%get_num_children() > 0)
      if (layout%has_import) then
         virtual_pt = VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, MAPL_FRAMEWORK_NAMESPACE // 'geom_in')
         item_spec = make_geometry_carrier_spec(ESMF_STATEINTENT_IMPORT)
         call this%registry%add_primary_spec(virtual_pt, item_spec, _RC)
         primary => this%registry%get_primary_spec(virtual_pt, _RC)
         call primary%create(_RC)
      end if
      if (component_spec%geometry_spec%kind == GEOMETRY_PROVIDER) then
         virtual_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, MAPL_FRAMEWORK_NAMESPACE // 'geom_out')
         item_spec = make_geometry_carrier_spec(ESMF_STATEINTENT_EXPORT, this%geom, this%vertical_grid)
         call this%registry%add_primary_spec(virtual_pt, item_spec, _RC)
         primary => this%registry%get_primary_spec(virtual_pt, _RC)
         call primary%create(_RC)
      end if
      if (component_spec%geometry_spec%kind == GEOMETRY_FROM_CHILD) then
         call component_spec%reexport(src_comp=component_spec%geometry_spec%provider, &
              src_name=MAPL_FRAMEWORK_NAMESPACE // 'geom_out', &
              new_name=MAPL_FRAMEWORK_NAMESPACE // 'geom_out', _RC)
      end if
      _RETURN(_SUCCESS)
   end subroutine advertise_framework_geometry

   subroutine advertise_state_items(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      call self_advertise(this, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine advertise_state_items

   subroutine self_advertise(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      associate (e => this%component_spec%var_specs%end())
         iter = this%component_spec%var_specs%begin()
         do while (iter /= e)
            var_spec => iter%of()
            call this%advertise_variable(var_spec, _RC)
            call iter%next()
         end do
      end associate
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine self_advertise

   subroutine run_advertise_callbacks(this, phase_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: phase_name
      integer, optional, intent(out) :: rc
      integer :: status
      call this%run_custom(ESMF_METHOD_INITIALIZE, phase_name, _RC)
      _RETURN(_SUCCESS)
   end subroutine run_advertise_callbacks

   subroutine activate_connections(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c

      associate (e => this%component_spec%connections%end())
         iter = this%component_spec%connections%begin()
         do while (iter /= e)
            c => iter%of()
            call c%activate(this%registry, _RC)
            call iter%next()
         end do
      end associate
      _RETURN(_SUCCESS)
   end subroutine activate_connections

end submodule initialize_advertise_smod
