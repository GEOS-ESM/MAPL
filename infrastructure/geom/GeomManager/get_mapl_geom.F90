#include "MAPL.h"

submodule (mapl_GeomManager_mod) get_mapl_geom_smod

   use mapl_GeomUtilities_mod, only: GeomGetId
   use mapl_GeomId_mod, only: GeomId

   implicit none

contains
   
   module function get_mapl_geom(geom, rc) result(mapl_geom)
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc
      type(MaplGeom), pointer :: mapl_geom

      type(GeomManager), pointer :: geom_mgr
       type(GeomId) :: id
       integer :: status

      geom_mgr => get_geom_manager()
       id = GeomGetId(geom, _RC)
      mapl_geom => geom_mgr%get_mapl_geom(id, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom

end submodule get_mapl_geom_smod
