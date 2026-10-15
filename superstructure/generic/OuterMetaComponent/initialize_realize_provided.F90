#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_realize_provided_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_REALIZE_PROVIDED
   use mapl_MultiState_mod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_realize_provided(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE_PROVIDED'

      call recurse(this, phase_idx=MAPL_GENERIC_INIT_REALIZE_PROVIDED, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

      call this%registry%allocate(_RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_realize_provided

end submodule initialize_realize_provided_smod
