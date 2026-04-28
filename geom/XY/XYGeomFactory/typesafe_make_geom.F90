#include "MAPL.h"

submodule (mapl3g_XYGeomFactory) typesafe_make_geom_smod
   use mapl_ErrorHandlingMod
   use mapl_InternalConstants
   use esmf
   implicit none

contains

   module function typesafe_make_geom(spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      type(XYGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      character(:), allocatable :: name

      if (spec%has_name()) name = spec%get_name()
      grid = create_basic_grid(spec, _RC)

      select case (spec%get_coord_mode())
      case (XY_COORD_ABI)
         call fill_coordinates_abi(spec, grid, _RC)
      case default
         call fill_coordinates(spec, grid, _RC)
      end select

      call add_mask(spec, grid, _RC)

      geom = ESMF_GeomCreate(grid=grid, _RC)

      _RETURN(_SUCCESS)
   end function typesafe_make_geom

end submodule typesafe_make_geom_smod
