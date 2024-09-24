#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) fill_coordinates_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_LatLonDecomposition
   use mapl3g_LatLonGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none (type, external)


contains

   module subroutine fill_coordinates(spec, grid, unusable, rc)
      type(LatLonGeomSpec), intent(in) :: spec
      type(ESMF_Grid), intent(inout) :: grid
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real(kind=ESMF_KIND_R8), pointer :: corners(:,:)
      integer :: i, j
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(LonAxis) :: local_lon_axis
      type(LatAxis) :: local_lat_axis
      type(LatLonDecomposition) :: decomp
      integer :: nx, ny, ix, iy

      lon_axis = spec%get_lon_axis()
      lat_axis = spec%get_lat_axis()
      decomp = spec%get_decomposition()

      nx = size(decomp%get_lon_distribution())
      ny = size(decomp%get_lat_distribution())
      call get_ranks(nx, ny, ix, iy, _RC)
 
     ! First we handle longitudes:
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, _RC)
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           farrayPtr=corners, _RC)

      lon_axis = spec%get_lon_axis()
      local_lon_axis = decomp%get_lon_subset(lon_axis, rank=ix)
      do j = 1, size(centers,2)
         centers(:,j) = local_lon_axis%get_centers()
      end do
      do j = 1, size(corners,2)
         corners(:,j) = local_lon_axis%get_corners()
      end do
      centers = centers * MAPL_DEGREES_TO_RADIANS_R8
      corners = corners * MAPL_DEGREES_TO_RADIANS_R8


      ! Now latitudes
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, _RC)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           farrayPtr=corners, _RC)

      local_lat_axis = decomp%get_lat_subset(lat_axis, rank=iy)
      do i = 1, size(centers,1)
         centers(i,:) = local_lat_axis%get_centers()
      end do
      do i = 1, size(corners,1)
         corners(i,:) = local_lat_axis%get_corners()
      end do

      centers = centers * MAPL_DEGREES_TO_RADIANS_R8
      corners = corners * MAPL_DEGREES_TO_RADIANS_R8

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      
      CONTAINS

      subroutine get_ranks(nx, ny, ix, iy, rc)
      integer, intent(in) :: nx, ny
      integer, intent(out) :: ix, iy
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: petCount, localPet
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, _RC)

      ix = mod(localPet, nx)
      iy = localPet / nx

      _RETURN(_SUCCESS)
      end subroutine get_ranks

   end subroutine fill_coordinates

end submodule fill_coordinates_smod
