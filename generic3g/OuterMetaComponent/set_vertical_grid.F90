#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) set_vertical_grid_smod
   implicit none

contains

   module subroutine set_vertical_grid(this, vertical_grid)
      class(OuterMetaComponent), intent(inout) :: this
      class(VerticalGrid), intent(in) :: verticaL_grid

      this%vertical_grid = vertical_grid

   end subroutine set_vertical_grid

end submodule set_vertical_grid_smod
