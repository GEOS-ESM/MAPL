#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) grid_get_coords_1d_smod
contains


   ! GridGetCoords - specific procedures
   module subroutine grid_get_coords_1d(grid, longitudes, latitudes, rc)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_loc
      type(ESMF_Grid), intent(in) :: grid
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), dimension(:,:), pointer :: lons_2d, lats_2d
      type(c_ptr) :: loc

      call GridGetCoords(grid, lons_2d, lats_2d, _RC)

      associate (n => product(shape(lons_2d)))
        loc = c_loc(lons_2d)
        call c_f_pointer(loc, longitudes, [n])

        loc = c_loc(lats_2d)
        call c_f_pointer(loc, latitudes, [n])
      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_coords_1d

end submodule grid_get_coords_1d_smod
