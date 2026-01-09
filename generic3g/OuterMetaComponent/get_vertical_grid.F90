#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_vertical_grid_smod

   implicit none

contains

   module function get_vertical_grid(this) result(vertical_grid)
      class(VerticalGrid), pointer :: verticaL_grid
      class(OuterMetaComponent), target, intent(inout) :: this

      verticaL_grid => null()
      if (allocated(this%verticaL_grid)) then
         vertical_grid => this%vertical_grid
      end if

   end function get_vertical_grid

end submodule get_vertical_grid_smod
