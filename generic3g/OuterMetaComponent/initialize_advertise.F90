#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_advertise_smod
   use mapl3g_GenericPhases, only: GENERIC_INIT_ADVERTISE
   use mapl3g_VirtualConnectionPt
   use mapl3g_StateItem
   use mapl3g_VariableSpec
   use mapl3g_VariableSpecVector, only: VariableSpecVectorIterator
   use esmf, only: operator(==)
   use mapl3g_Connection
   use mapl3g_ConnectionVector, only: ConnectionVectorIterator
   use mapl3g_ConnectionVector, only: operator(/=)
   use mapl3g_VariableSpecVector, only: operator(/=)
   use mapl3g_StateItemSpec
   use mapl3g_Multistate
   use mapl3g_stateItemExtension
   use mapl_ErrorHandling
   implicit none (type, external)


contains

   module recursive subroutine initialize_advertise(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(MultiState) :: user_states, tmp_states
      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE'

      call recurse(this, phase_idx=GENERIC_INIT_ADVERTISE, _RC)
      call self_advertise(this, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      call process_connections(this, _RC)
      call this%registry%propagate_unsatisfied_imports(_RC)
      call this%registry%propagate_exports(_RC)

      user_states = this%user_gc_driver%get_states()
      tmp_states = MultiState(exportState=user_states%exportState, internalState=user_states%internalState)
      call this%registry%add_to_states(tmp_states, mode='user', _RC)
      ! Destroy the temporary states
      call ESMF_StateDestroy(tmp_states%importState, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_advertise

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
              call advertise_variable( this, var_spec, _RC)
              call iter%next()
           end do
         end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine self_advertise

   subroutine advertise_variable(this, var_spec, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(StateItemSpec), target :: item_spec
      type(StateItemSpec), pointer :: item_spec_ptr
      type(StateItemExtension), pointer :: item_extension
      type(VirtualConnectionPt) :: virtual_pt
      
      item_spec = var_spec%make_StateItemSpec(this%registry, &
           this%geom, this%vertical_grid, timestep=this%user_timestep, offset=this%user_offset, _RC)
      virtual_pt = var_spec%make_virtualPt()
      call this%registry%add_primary_spec(virtual_pt, item_spec)
      item_extension => this%registry%get_primary_extension(virtual_pt, _RC)
      item_spec_ptr => item_extension%get_spec() 
      call item_spec_ptr%create(_RC)
      
      if (this%component_spec%misc%activate_all_exports) then
         if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT) then
            call item_spec_ptr%activate(_RC)
         end if
      end if
      if (this%component_spec%misc%activate_all_imports) then
         if (var_spec%state_intent == ESMF_STATEINTENT_IMPORT) then
            call item_spec_ptr%activate(_RC)
         end if
      end if
      
      if (var_spec%state_intent == ESMF_STATEINTENT_INTERNAL) then
         call item_spec_ptr%activate(_RC)
      end if
      
      _RETURN(_SUCCESS)
   end subroutine advertise_variable

   subroutine process_connections(this, rc)
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
   end subroutine process_connections

end submodule initialize_advertise_smod
