#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) set_geom_smod
   implicit none

contains

   module subroutine set_geom(this, geom)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom

      _HERE, this%get_name()
      this%geom = geom
      _HERE, allocated(this%geom)

   end subroutine set_geom

end submodule set_geom_smod
