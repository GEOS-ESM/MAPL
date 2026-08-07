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
   use mapl_ErrorHandling_mod
   implicit none (type, external)


contains

   module recursive subroutine initialize_advertise(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
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

   subroutine advertise_framework_geometry(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      ! Geometry carriers remain framework-only until geometry propagation is enabled.
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
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
