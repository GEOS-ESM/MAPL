#include "MAPL.h"

submodule (mapl_XYGeomSpec_mod) make_geom_smod
   use mapl_ErrorHandling_mod
   use mapl_InternalConstants_mod
   use esmf
   implicit none

contains

   module function make_geom(this, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(XYGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      character(:), allocatable :: name

      if (this%has_name()) name = this%get_name()
      grid = create_basic_grid(this, _RC)

      select case (this%get_coord_mode())
      case (XY_COORD_ABI)
         call fill_coordinates_abi(this, grid, _RC)
      case default
         call fill_coordinates(this, grid, _RC)
      end select

      call add_mask(this, grid, _RC)

      geom = ESMF_GeomCreate(grid=grid, _RC)

      _RETURN(_SUCCESS)
   end function make_geom

end submodule make_geom_smod
