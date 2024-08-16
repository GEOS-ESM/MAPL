#include "MAPL_ErrLog.h"

submodule(mapl3g_GriddedComponentDriver) clock_advance_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl3g_MethodPhasesMapUtils
   use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_INVALIDATE, GENERIC_COUPLER_UPDATE
   implicit none

contains

   module subroutine clock_advance(this, rc)
      class(GriddedComponentDriver), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_ClockAdvance(this%clock, _RC)

      _RETURN(_SUCCESS)
   end subroutine clock_advance

end submodule clock_advance_smod
