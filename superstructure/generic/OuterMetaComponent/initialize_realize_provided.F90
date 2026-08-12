#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_realize_provided_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_REALIZE_PROVIDED
   use mapl_MultiState_mod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_realize_provided(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE_PROVIDED'
      type(MultiState) :: user_states

      ! Providers must publish metadata before parent components analyze it.
      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_REALIZE_PROVIDED, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call this%registry%propagate_exports(_RC)

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_realize_provided

end submodule initialize_realize_provided_smod
