#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_modify_advertised_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_MODIFY_ADVERTISED
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_modify_advertised(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_MODIFY_ADVERTISED'
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_MODIFY_ADVERTISED, _RC)

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize_modify_advertised

end submodule initialize_modify_advertised_smod
