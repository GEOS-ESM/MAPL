#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_modify_advertised2_smod
   use mapl3g_Multistate
   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine initialize_modify_advertised2(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_MODIFY_ADVERTISED2'
      type(MultiState) :: outer_states, user_states, tmp_states

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call this%registry%propagate_exports(_RC)
      call recurse(this, phase_idx=GENERIC_INIT_MODIFY_ADVERTISED2, _RC)

      user_states = this%user_gc_driver%get_states()
      tmp_states = MultiState(importState=user_states%importState)
      call this%registry%add_to_states(tmp_states, mode='user', _RC)
      outer_states = MultiState(importState=importState, exportState=exportState)
      call this%registry%add_to_states(outer_states, mode='outer', _RC)

      ! Clean up
      call ESMF_StateDestroy(tmp_states%exportState, _RC)
      call ESMF_StateDestroy(tmp_states%internalState, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(clock)
   end subroutine initialize_modify_advertised2
   
end submodule initialize_modify_advertised2_smod
