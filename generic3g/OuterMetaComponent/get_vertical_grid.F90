#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_vertical_grid_smod

   implicit none

contains

   module function get_vertical_grid(this) result(vertical_grid)
      class(VerticalGrid), allocatable :: verticaL_grid
      class(OuterMetaComponent), intent(inout) :: this
      vertical_grid = this%vertical_grid
   end function get_vertical_grid

end submodule get_vertical_grid_smod
