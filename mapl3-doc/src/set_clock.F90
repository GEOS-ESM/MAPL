#include "MAPL_ErrLog.h"

submodule(mapl3g_GriddedComponentDriver) set_clock_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl3g_MethodPhasesMapUtils
   use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_INVALIDATE, GENERIC_COUPLER_UPDATE
   implicit none

contains

   module subroutine set_clock(this, clock)
      class(GriddedComponentDriver), intent(inout) :: this
      type(ESMF_Clock), intent(in) :: clock

      this%clock = clock
   end subroutine set_clock

end submodule set_clock_smod
