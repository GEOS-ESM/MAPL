#include "MAPL.h"
submodule (mapl_LatLonGeomFactory_mod) typesafe_make_geom_smod
   use mapl_GeomSpec_mod
   use mapl_LonAxis_mod
   use mapl_LatAxis_mod
   use mapl_LatLonDecomposition_mod
   use mapl_LatLonGeomSpec_mod
   use mapl_MinMax_mod
   use mapl_ErrorHandling_mod
   use mapl_Constants_mod
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer
   implicit none (type, external)


contains

   module function typesafe_make_geom(spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(LatLonGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      character(:), allocatable :: name

      if (spec%has_name()) name = spec%get_name()
      grid = create_basic_grid(spec, name=name, _RC)
      call fill_coordinates(spec, grid, _RC)
      geom = ESMF_GeomCreate(grid=grid, _RC)

      _RETURN(_SUCCESS)
   end function typesafe_make_geom

end submodule typesafe_make_geom_smod
