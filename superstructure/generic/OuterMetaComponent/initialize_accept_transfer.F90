#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_accept_transfer_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_ACCEPT_TRANSFER
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

      integer :: status

      call this%run_custom(ESMF_METHOD_INITIALIZE, 'GENERIC::INIT_ACCEPT_TRANSFER', _RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_ACCEPT_TRANSFER, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_accept_transfer

end submodule initialize_accept_transfer_smod
