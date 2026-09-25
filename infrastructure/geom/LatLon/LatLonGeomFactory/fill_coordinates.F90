#include "MAPL.h"
submodule (mapl_LatLonGeomFactory_mod) fill_coordinates_smod
   use mapl_GeomSpec_mod
   use mapl_LonAxis_mod
   use mapl_LatAxis_mod
   use mapl_LatLonDecomposition_mod
   use mapl_LatLonGeomSpec_mod
   use mapl_MinMax_mod
   use mapl_ErrorHandling_mod
   use MAPL_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer
   use mapl_GridAccessors_mod, only: grid_has_de, grid_get_interior
   implicit none (type, external)


contains

   module subroutine fill_coordinates(spec, grid, unusable, rc)
      use mapl_KeywordEnforcer_mod
      type(LatLonGeomSpec), intent(in) :: spec
      type(ESMF_Grid), intent(inout) :: grid
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real(kind=ESMF_KIND_R8), pointer :: corners(:,:)
      real(kind=ESMF_KIND_R8),  allocatable :: lon_centers(:), lat_centers(:), lon_corners(:), lat_corners(:)
      integer :: i, j
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      logical :: has_de
      integer, allocatable :: grid_interior_centers(:), grid_interior_corners(:)
      integer :: im_world, jm_world

      lon_axis = spec%get_lon_axis()
      lat_axis = spec%get_lat_axis()

      has_de = grid_has_de(grid, _RC)
      if (.not. has_de) then
         _RETURN(_SUCCESS)
      end if

      lon_axis = spec%get_lon_axis()
      lat_axis = spec%get_lat_axis()
      lon_centers = lon_axis%get_centers()
      lat_centers = lat_axis%get_centers()
      im_world = size(lon_centers)
      jm_world = size(lat_centers)
      lon_corners = lon_axis%get_corners()
      lat_corners = lat_axis%get_corners()
      call grid_get_interior(grid, grid_interior_centers, _RC) 
      allocate(grid_interior_corners(size(grid_interior_centers)), _STAT)

 
      grid_interior_corners=grid_interior_centers
      if (.not.lon_axis%is_periodic()) then
         if (grid_interior_centers(2) == im_world) grid_interior_corners(2) = grid_interior_centers(2)+1
      end if

      if (grid_interior_centers(4) == jm_world) grid_interior_corners(4) = grid_interior_centers(4)+1

     ! First we handle longitudes:
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, _RC)
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           farrayPtr=corners, _RC)

      do j = 1, size(centers,2)
         centers(:,j) = lon_centers(grid_interior_centers(1):grid_interior_centers(2))
      end do
      do j = 1, size(corners,2)
         corners(:,j) = lon_corners(grid_interior_corners(1):grid_interior_corners(2))
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

      do i = 1, size(centers,1)
         centers(i,:) = lat_centers(grid_interior_centers(3):grid_interior_centers(4)) 
      end do
      do i = 1, size(corners,1)
         corners(i,:) = lat_corners(grid_interior_corners(3):grid_interior_corners(4)) 
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
