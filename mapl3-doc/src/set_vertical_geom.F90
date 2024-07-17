#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) set_vertical_geom_smod
   implicit none

contains

   module subroutine set_vertical_geom(this, vertical_geom)
      class(OuterMetaComponent), intent(inout) :: this
      type(VerticalGeom), intent(in) :: verticaL_geom

      this%vertical_geom = vertical_geom

   end subroutine set_vertical_geom

end submodule set_vertical_geom_smod
