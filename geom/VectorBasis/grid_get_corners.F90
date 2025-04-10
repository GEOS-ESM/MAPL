#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) grid_get_corners_smod
   use mapl_base, only: MAPL_GridGetCorners
contains


   module subroutine grid_get_corners(grid, corners, rc)
      type(ESMF_Grid), intent(inout) :: grid
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: corners(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: im, jm
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corner_lons(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corner_lats(:,:)

      call GridGetCoords(grid, longitudes, latitudes, _RC)
      im = size(longitudes,1)
      jm = size(longitudes,2)

      allocate(corner_lons(im+1,jm+1))
      allocate(corner_lats(im+1,jm+1))

      call MAPL_GridGetCorners(grid, corner_lons, corner_lats, _RC)

      allocate(corners(size(longitudes,1),size(longitudes,2),2))
      corners(:,:,1) = corner_lons
      corners(:,:,2) = corner_lats

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_corners
   
end submodule grid_get_corners_smod
