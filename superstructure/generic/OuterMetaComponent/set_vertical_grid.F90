#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) set_vertical_grid_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module subroutine set_vertical_grid(this, vertical_grid, rc)
      class(OuterMetaComponent), intent(inout) :: this
      class(VerticalGrid), intent(in) :: verticaL_grid
      integer, optional, intent(out) :: rc

      integer :: status

       this%vertical_grid = vertical_grid
       call this%registry%set_geometry(vertical_grid=vertical_grid, _RC)

       if (present(rc)) rc = ESMF_SUCCESS

   end subroutine set_vertical_grid

end submodule set_vertical_grid_smod
