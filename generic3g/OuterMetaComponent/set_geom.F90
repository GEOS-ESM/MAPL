#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) set_geom_smod
   implicit none

contains

   module subroutine set_geom(this, geom)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom

      this%geom = geom

   end subroutine set_geom

end submodule set_geom_smod
