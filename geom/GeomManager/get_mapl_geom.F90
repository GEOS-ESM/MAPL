#include "MAPL.h"

submodule (mapl3g_GeomManager) get_mapl_geom_smod

   use mapl3g_GeomUtilities, only: MAPL_GeomGetId

   implicit none

contains
   
   module function get_mapl_geom(geom, rc) result(mapl_geom)
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc
      type(MaplGeom), pointer :: mapl_geom

      type(GeomManager), pointer :: geom_mgr
      integer :: id, status

      geom_mgr => get_geom_manager()
      id = MAPL_GeomGetId(geom, _RC)
      mapl_geom => geom_mgr%get_mapl_geom(id, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom

end submodule get_mapl_geom_smod
