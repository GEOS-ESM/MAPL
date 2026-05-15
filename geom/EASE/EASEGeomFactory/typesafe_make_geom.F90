#include "MAPL.h"

submodule (mapl3g_EASEGeomFactory) typesafe_make_geom_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEGeomSpec
   use mapl_ErrorHandlingMod
   use esmf

   implicit none (type, external)

contains

   module function typesafe_make_geom(spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      type(EASEGeomSpec), intent(in) :: spec
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
