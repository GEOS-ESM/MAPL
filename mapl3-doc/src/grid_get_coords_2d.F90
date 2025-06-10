#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) grid_get_coords_2d_smod
   use mapl_base, only: MAPL_GridGetCorners
contains

   module subroutine grid_get_coords_2d(grid, longitudes, latitudes, rc)
      type(ESMF_Grid), intent(in) :: grid
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:,:)
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, farrayPtr=longitudes, &
           staggerloc=ESMF_STAGGERLOC_CENTER, _RC)
      call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, farrayPtr=latitudes, &
           staggerloc=ESMF_STAGGERLOC_CENTER, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_coords_2d

end submodule grid_get_coords_2d_smod
