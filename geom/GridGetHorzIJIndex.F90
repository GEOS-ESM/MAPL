! `MAPL_GetHorzIJIndex` -- Get indexes on destributed ESMF grid for an arbitary lat and lon
!
! For a set of longitudes and latitudes in radians this routine will return the indexes for the
! domain
! Depending on how it is invoked these will be the local domain or the global indices.
! If the Lat/Lon pair is not in the domain -1 is returned.
! The routine works for both the gmao cube and lat/lon grids.
! Currently the lat/lon grid is asumed to go from -180 to 180

#include "MAPL.h"

module mapl3g_GridGetHorzIJIndex

   use ESMF, only: ESMF_KIND_R8, ESMF_Grid
   use MAPL_ErrorHandling, only: MAPL_Assert

   implicit none(type, external)
   private

   public :: GridGetHorzIJIndex

   interface GridGetHorzIJIndex
      module procedure get_horz_ij_index
   end interface GridGetHorzIJIndex

contains

   subroutine get_horz_ij_index(npts, ii, jj, lon, lat, lonR8, latR8, grid, rc)

      integer, intent(in) :: npts ! number of points in lat and lon arrays
      integer, intent(inout) :: ii(npts) ! array of the first index for each lat and lon
      integer, intent(inout) :: jj(npts) ! array of the second index for each lat and lon
      real, optional, intent(in) :: lon(npts) ! array of longitudes in radians
      real, optional, intent(in) :: lat(npts) ! array of latitudes in radians
      real(kind=ESMF_KIND_R8), optional, intent(in) :: lonR8(npts) ! array of longitudes in radians
      real(kind=ESMF_KIND_R8), optional, intent(in) :: latR8(npts) ! array of latitudes in radians
      type(ESMF_Grid), optional, intent(inout) :: grid ! ESMF grid
      integer, optional, intent(out) :: rc ! return code

      ii = -1
      jj = -1
      _FAIL("Use GeomGetHorzIJIndex instead")

      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
      _UNUSED_DUMMY(lonR8)
      _UNUSED_DUMMY(latR8)
   end subroutine get_horz_ij_index

end module mapl3g_GridGetHorzIJIndex
