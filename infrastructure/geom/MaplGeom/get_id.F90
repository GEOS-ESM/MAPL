#include "MAPL.h"

submodule (mapl_MaplGeom_mod) get_id_smod

   implicit none

contains

   module function get_id(this) result(id)
      use mapl_GeomId_mod, only: GeomId
      type(GeomId) :: id
      class(MaplGeom), intent(in) :: this

      id = this%geom_id
   end function get_id

end submodule get_id_smod
