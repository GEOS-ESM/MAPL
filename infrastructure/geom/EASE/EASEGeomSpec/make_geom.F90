#include "MAPL.h"

submodule (mapl_EASEGeomSpec_mod) make_geom_smod
   use mapl_GeomSpec_mod
   use mapl_ErrorHandling_mod
   use esmf
   implicit none (type, external)

contains

   module function make_geom(this, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(EASEGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      character(:), allocatable :: name

      if (this%has_name()) name = this%get_name()
      grid = create_basic_grid(this, name=name, _RC)
      call fill_coordinates(this, grid, _RC)
      geom = ESMF_GeomCreate(grid=grid, _RC)

      _RETURN(_SUCCESS)
   end function make_geom

end submodule make_geom_smod
