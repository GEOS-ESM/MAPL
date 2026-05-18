#include "MAPL.h"

submodule (mapl3g_GridGet) grid_get_centers_smod

   use mapl3g_VectorBasis, only: GridGetCoords

   implicit none(type, external)

contains

   module subroutine grid_get_centers(grid, centers, rc)
      type(ESMF_Grid), intent(in) :: grid
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: centers(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:,:)

      call GridGetCoords(grid, longitudes, latitudes, _RC)

      allocate(centers(size(longitudes,1),size(longitudes,2),2))
      centers(:,:,1) = longitudes
      centers(:,:,2) = latitudes

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_centers

end submodule grid_get_centers_smod
