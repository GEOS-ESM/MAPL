#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_realize_accepted_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_REALIZE_ACCEPTED
   use mapl_MultiState_mod
   use mapl_GraphBuilder_mod, only: GraphBuilder
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_realize_accepted(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE_ACCEPTED'
      type(MultiState) :: outer_states, user_states, tmp_states
      type(GraphBuilder) :: gb

      call recurse(this, phase_idx=MAPL_GENERIC_INIT_REALIZE_ACCEPTED, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      call this%registry%allocate(_RC)
      call gb%allocate_internals(this, _RC)

      if (.not. graph_populates_user_states()) then
         user_states = this%user_gc_driver%get_states()
         tmp_states = MultiState(importState=user_states%importState)
         call this%registry%add_to_states(tmp_states, mode='user', _RC)
      end if

      outer_states = MultiState(importState=importState, exportState=exportState)
      if (graph_populates_user_states()) then
         call gb%run_populate_outer_hook(this, outer_states)
      else
         call this%registry%add_to_states(outer_states, mode='outer', _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_realize_accepted

end submodule initialize_realize_accepted_smod
