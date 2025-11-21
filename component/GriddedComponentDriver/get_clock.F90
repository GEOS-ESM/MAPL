#include "MAPL.h"

submodule(mapl3g_GriddedComponentDriver) get_clock_smod
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   module function get_clock(this) result(clock)
      type(ESMF_Clock) :: clock
      class(GriddedComponentDriver), intent(in) :: this

      clock = this%clock
   end function get_clock

end submodule get_clock_smod
