#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) get_mapl_geom_from_id_smod

   implicit none

contains
   
   module function get_mapl_geom_from_id(this, id, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status

      mapl_geom => this%mapl_geoms%at(id, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_id

end submodule get_mapl_geom_from_id_smod
