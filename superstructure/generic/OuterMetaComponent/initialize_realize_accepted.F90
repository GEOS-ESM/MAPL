#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_realize_accepted_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_REALIZE_ACCEPTED
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
       call recurse(this, phase_idx=MAPL_GENERIC_INIT_REALIZE_ACCEPTED, _RC)
       call this%registry%allocate(_RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_realize_accepted

end submodule initialize_realize_accepted_smod
