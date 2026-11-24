#include "MAPL.h"

submodule (mapl_GeomManager_mod) has_mapl_geom_smod

   implicit none

contains

   module function has_mapl_geom(this, id) result(has_geom)
      logical :: has_geom
      class(GeomManager), target, intent(in) :: this
      type(GeomId), intent(in) :: id

      has_geom = this%mapl_geoms%count(id%get_value()) > 0

   end function has_mapl_geom

end submodule has_mapl_geom_smod
