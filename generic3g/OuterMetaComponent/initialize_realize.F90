#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_realize_smod
   use mapl3g_Multistate
   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine initialize_realize(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE'
      type(MultiState) :: outer_states, user_states, tmp_states

      call recurse(this, phase_idx=GENERIC_INIT_REALIZE, _RC)

      user_states = this%user_gc_driver%get_states()
      tmp_states = MultiState(importState=user_states%importState)
      call this%registry%add_to_states(tmp_states, mode='user', _RC)
      outer_states = MultiState(importState=importState, exportState=exportState)
      call this%registry%add_to_states(outer_states, mode='outer', _RC)

      call this%registry%allocate(_RC)

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_realize

end submodule initialize_realize_smod
