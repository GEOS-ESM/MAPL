#include "MAPL.h"

submodule (mapl3g_EASEGeomFactory) fill_coordinates_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEGeomSpec
   use mapl_ErrorHandlingMod
   use mapl_KeywordEnforcer, only: KeywordEnforcer
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none (type, external)

contains

   ! Fill center and corner coordinate arrays into the ESMF_Grid.
   ! EASE grids store 2D coordinates (coordDep1/2 = [1,2]), so both
   ! lon and lat arrays are 2D even though they vary only along one axis.
   module subroutine fill_coordinates(spec, grid, unusable, rc)
      type(EASEGeomSpec), intent(in) :: spec
      type(ESMF_Grid), intent(inout) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, i, j
      integer :: lbound_cen(2), ubound_cen(2)   ! interior bounds
      integer :: lbound_cor(2), ubound_cor(2)   ! corner bounds
      integer :: i1, in, j1, jn
      integer :: ic1, icn, jc1, jcn
      real(kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real(kind=ESMF_KIND_R8), pointer :: corners(:,:)
      real(kind=REAL64), allocatable :: lon_cen(:), lat_cen(:)
      real(kind=REAL64), allocatable :: lon_cor(:), lat_cor(:)

      lon_cen = spec%get_lon_centers()
      lat_cen = spec%get_lat_centers()
      lon_cor = spec%get_lon_corners()
      lat_cor = spec%get_lat_corners()

      ! Query local DE bounds using proper allocatable arrays (not array constructors)
      call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
           & exclusiveLBound=lbound_cen, exclusiveUBound=ubound_cen, _RC)
      call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CORNER, &
           & exclusiveLBound=lbound_cor, exclusiveUBound=ubound_cor, _RC)

      i1  = lbound_cen(1); in  = ubound_cen(1)
      j1  = lbound_cen(2); jn  = ubound_cen(2)
      ic1 = lbound_cor(1); icn = ubound_cor(1)
      jc1 = lbound_cor(2); jcn = ubound_cor(2)

      ! Fill longitude centers
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           & staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=centers, _RC)
      do j = 1, size(centers, 2)
         centers(:, j) = lon_cen(i1:in)
      end do

      ! Fill longitude corners
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           & staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=corners, _RC)
      do j = 1, size(corners, 2)
         corners(:, j) = lon_cor(ic1:icn)
      end do

      ! Fill latitude centers
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           & staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=centers, _RC)
      do i = 1, size(centers, 1)
         centers(i, :) = lat_cen(j1:jn)
      end do

      ! Fill latitude corners
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           & staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=corners, _RC)
      do i = 1, size(corners, 1)
         corners(i, :) = lat_cor(jc1:jcn)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine fill_coordinates

end submodule fill_coordinates_smod
