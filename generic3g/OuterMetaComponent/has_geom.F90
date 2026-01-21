#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) has_geom_smod

   implicit none

contains

   module function has_geom(this)
      logical :: has_geom
      class(OuterMetaComponent), intent(in) :: this

      has_geom = allocated(this%geom)

   end function has_geom

end submodule has_geom_smod
