#include "MAPL_ErrLog.h"

submodule(mapl3g_GriddedComponentDriver) get_clock_smod
   use :: mapl_ErrorHandling
   use :: mapl3g_OuterMetaComponent
   use :: mapl3g_MethodPhasesMapUtils
   use mapl3g_CouplerMetaComponent, only: GENERIC_COUPLER_INVALIDATE, GENERIC_COUPLER_UPDATE
   implicit none

contains

   module function get_clock(this) result(clock)
      type(ESMF_Clock) :: clock
      class(GriddedComponentDriver), intent(in) :: this

      clock = this%clock
   end function get_clock

end submodule get_clock_smod
