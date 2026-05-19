#include "MAPL.h"

submodule(mapl3g_GriddedComponentDriver) set_clock_smod
   implicit none(type,external)

contains

   module subroutine set_clock(this, clock)
      class(GriddedComponentDriver), intent(inout) :: this
      type(ESMF_Clock), intent(in) :: clock

      this%clock = clock
   end subroutine set_clock

end submodule set_clock_smod
