#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_post_advertise_smod
   implicit none

contains

   module recursive subroutine initialize_post_advertise(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_POST_ADVERTISE'
      type(MultiState) :: outer_states, user_states

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)

      outer_states = MultiState(importState=importState, exportState=exportState)
      call this%registry%add_to_states(outer_states, mode='outer', _RC)

      call recurse(this, phase_idx=GENERIC_INIT_POST_ADVERTISE, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_post_advertise

end submodule initialize_post_advertise_smod
