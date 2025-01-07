#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) typesafe_make_geom_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_LatLonDecomposition
   use mapl3g_LatLonGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none (type, external)


contains

   module function typesafe_make_geom(spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(LatLonGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid

      grid = create_basic_grid(spec, _RC)
      call fill_coordinates(spec, grid, _RC)
      geom = ESMF_GeomCreate(grid=grid, _RC)

      _RETURN(_SUCCESS)
   end function typesafe_make_geom

end submodule typesafe_make_geom_smod
