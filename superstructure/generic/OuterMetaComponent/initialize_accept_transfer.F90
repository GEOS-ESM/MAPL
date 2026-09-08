#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_accept_transfer_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_ACCEPT_TRANSFER
   use mapl_GeometrySpec_mod
   use mapl_MultiState_mod
   use mapl_Connection_mod
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_accept_transfer(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(MultiState) :: outer_states, user_states, tmp_states
      class(GriddedComponentDriver), pointer :: provider
      type(ESMF_GridComp) :: provider_gc
      type(OuterMetaComponent), pointer :: provider_meta
      integer :: status

!#      call this%propagate_geom_to_children(_RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_ACCEPT_TRANSFER, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, 'GENERIC::INIT_ACCEPT_TRANSFER', _RC)

      call process_connections(this, _RC)
      call this%registry%propagate_exports(_RC)

      user_states = this%user_gc_driver%get_states()
      tmp_states = MultiState(importState=user_states%importState)
      call this%registry%add_to_states(tmp_states, mode='user', _RC)

      outer_states = MultiState(importState=importState, exportState=exportState)
      call this%registry%add_to_states(outer_states, mode='outer', _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_accept_transfer

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
           call c%connect(this%registry, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine process_connections

end submodule initialize_accept_transfer_smod
