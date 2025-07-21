#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_realize_smod
   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine initialize_realize(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE'

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_REALIZE, _RC)
      call this%registry%allocate(_RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_realize

end submodule initialize_realize_smod
