#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_geom_id_smod

   implicit none(type,external)

contains

   module function get_geom_id(this) result(geom_id)
      type(GeomId) :: geom_id
      class(OuterMetaComponent), intent(in) :: this

      geom_id = this%geom_id
   end function get_geom_id

end submodule get_geom_id_smod
