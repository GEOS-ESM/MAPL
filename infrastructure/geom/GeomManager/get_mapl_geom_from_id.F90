#include "MAPL.h"

submodule (mapl_GeomManager_mod) get_mapl_geom_from_id_smod
   use mapl_GeomId_mod, only: GeomId

   implicit none

contains
   
   module function get_mapl_geom_from_id(this, id, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
       type(GeomId), intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status

       mapl_geom => this%mapl_geoms%at(id%get_value(), _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_id

end submodule get_mapl_geom_from_id_smod
