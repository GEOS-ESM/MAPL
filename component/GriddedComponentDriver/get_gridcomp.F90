#include "MAPL.h"

submodule (mapl3g_GriddedComponentDriver) get_gridcomp_smod
   implicit none

contains

   module function get_gridcomp(this) result(gridcomp)
      use esmf, only: ESMF_GridComp
      type(ESMF_GridComp) :: gridcomp
      class(GriddedComponentDriver), intent(in) :: this
      gridcomp = this%gridcomp
   end function get_gridcomp

end submodule get_gridcomp_smod
