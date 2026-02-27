! `MAPL_GetHorzIJIndex` -- Get indexes on destributed ESMF grid for an arbitary lat and lon
!
! For a set of longitudes and latitudes in radians this routine will return the indexes for the
! domain
! Depending on how it is invoked these will be the local domain or the global indices.
! If the Lat/Lon pair is not in the domain -1 is returned.
! The routine works for both the gmao cube and lat/lon grids.
! Currently the lat/lon grid is asumed to go from -180 to 180

#include "MAPL.h"

module GetHorzIJIndex

   use ESMF, only: ESMF_KIND_R8, ESMF_MAXSTR
   use ESMF, only: ESMF_Grid, ESMF_GridGet, ESMF_GridGetCoord, ESMF_STAGGERLOC_CORNER
   use ESMF, only: ESMF_CoordSys_Flag, ESMF_COORDSYS_SPH_DEG, ESMF_COORDSYS_CART
   use ESMF, only: ESMF_Info, ESMF_InfoGetFromHost, ESMF_InfoIsPresent, ESMF_InfoGet
   use MAPL_Grid, only: MAPL_GridGet, MAPL_GridGetInterior, MAPL_GridGetCorners
   use MAPL_Utils, only: _ASSERT, _FAIL, _RETURN, _RETURN_IF, _SUCCESS, _STAT

   private
   implicit none

   public :: get_horz_ij_index

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

      integer :: im, jm, im_world, jm_world, counts(3), dims(3)
      real(kind=ESMF_KIND_R8), pointer :: lons(:, :)
      real(kind=ESMF_KIND_R8), pointer :: lats(:, :)
      real(kind=ESMF_KIND_R8), allocatable :: elons(:)
      real(kind=ESMF_KIND_R8), allocatable :: elats(:)
      integer :: i, iiloc, jjloc, i1, i2, j1, j2
      real(kind=ESMF_KIND_R4) :: lonloc, latloc
      logical :: local_search
      real(kind=ESMF_KIND_R8), allocatable :: tmp_lons(:), tmp_lats(:)
      type(ESMF_CoordSys_Flag) :: coord_sys
      ! character(len=ESMF_MAXSTR) :: grid_type
      ! type(ESMF_Info) :: infoh
      integer :: status

      ! if the grid is present then we can just get the prestored edges and the dimensions of the
      ! grid
      ! this also means we are running on a distributed grid
      ! if grid not present then the we just be running outside of ESMF and the user must
      ! pass in the the dimensions of the grid and we must compute them
      ! and assume search on the global domain
      if (present(grid)) then
         call MAPL_GridGet(grid, localCellCountPerDim=counts, globalCellCountPerDim=dims, _RC)
         im_world = dims(1)
         jm_world = dims(2)
         im = counts(1)
         jm = counts(2)
         local_search = .true.
      else
         local_search = .false.
      end if

      allocate(tmp_lons(npts), tmp_lats(npts))
      if (present(lon) .and. present(lat)) then
         tmp_lons = lon
         tmp_lats = lat
      else if (present(lonR8) .and. present(latR8)) then
         tmp_lons = lonR8
         tmp_lats = latR8
      end if

      ! call ESMF_InfoGetFromHost(grid,infoh,_RC)
      ! call ESMF_InfoGet(infoh, key='GridType', value=grid_type, _RC)
      ! if(trim(grid_type) == "Cubed-Sphere") then
      if (im_world * 6 == jm_world) then
         call MAPL_GetGlobalHorzIJIndex(npts, ii, jj, lon=lon, lat=lat, lonR8=lonR8, latR8=latR8, grid=grid, _RC)
         call MAPL_Grid_Interior(grid, i1, i2, j1, j2)
         ! convert index to local, if it is not in domain, set it to -1 just as the legacy code
         where (i1 <= ii .and. ii <= i2 .and. j1 <= jj .and. jj <= j2)
            ii = ii - i1 + 1
            jj = jj - j1 + 1
            elsewhere
            ii = -1
            jj = -1
         end where
      else
         _ASSERT(local_search, "Global Search for IJ for latlon not implemented")
         call ESMF_GridGetCoord(grid, coordDim=1, localDe=0, staggerloc=ESMF_STAGGERLOC_CORNER, fArrayPtr=lons, _RC)
         call ESMF_GridGetCoord(grid, coordDim=2, localDe=0, staggerloc=ESMF_STAGGERLOC_CORNER, fArrayPtr=lats, _RC)
         allocate(elons(im + 1), _STAT)
         allocate(elats(jm + 1), _STAT)
         call ESMF_GridGet(grid, coordSys=coord_sys, _RC)
         elons = lons(:, 1)
         elats = lats(1, :)
         if (coord_sys == ESMF_COORDSYS_SPH_DEG) then
            elons = elons * MAPL_DEGREES_TO_RADIANS_R8
            elats = elats * MAPL_DEGREES_TO_RADIANS_R8
         else if (coord_sys == ESMF_COORDSYS_CART) then
            _FAIL('Unsupported coordinate system:  ESMF_COORDSYS_CART')
         end if
         ! lat-lon grid goes from -180 to 180 shift if we must
         ! BMA this -180 to 180 might change at some point
         do i = 1, npts
            lonloc = tmp_lons(i)
            latloc = tmp_lats(i)
            if (lonloc > MAPL_PI) lonloc = lonloc - 2.0 * MAPL_PI
            iiloc = ijsearch(elons, im + 1, lonloc, .false.)
            jjloc = ijsearch(elats, jm + 1, latloc, .false.)
            ii(i) = iiloc
            jj(i) = jjloc
         end do
         deallocate(elons, elats)
      end if

      deallocate(tmp_lons, tmp_lats)
      _RETURN(_SUCCESS)

   contains

      integer function ijsearch(coords, idim, valueIn, periodic) ! fast bisection version
         implicit none
         real(kind=ESMF_KIND_R8), intent(in) :: coords(:)
         integer, intent(in) :: idim
         real, intent(inout) :: valueIn
         logical, intent(in) :: periodic

         integer :: i, i1, i2, k
         real :: value

         value = valueIn
         if (periodic) then
            if (value > coords(idim)) value = value - 360.
         end if

         ijsearch = -1
         i1 = 1
         i2 = idim
         if (coords(idim) > coords(1)) then
            do k = 1, idim ! it should never take take long
               i = (i1 + i2) / 2
               if ((value >= coords(i))) then
                  if (value < coords(i + 1)) then
                     ijsearch = i
                     exit
                  else
                     i1 = i
                  end if
               else
                  i2 = i
               end if
            end do
         else
            do k = 1, idim ! it should never take take long
               i = (i1 + i2) / 2
               if ((value < coords(i))) then
                  if (value >= coords(i + 1)) then
                     ijsearch = i
                     exit
                  else
                     i1 = i
                  end if
               else
                  i2 = i
               end if
            end do
         end if
      end function ijsearch

   end subroutine get_horz_ij_index

   subroutine get_global_horz_ij_index(npts, ii, jj, lon, lat, lonR8, latR8, grid, rc)

      integer, intent(in) :: npts ! number of points in lat and lon arrays
      integer, intent(inout) :: ii(npts) ! array of the first index for each lat and lon
      integer, intent(inout) :: jj(npts) ! array of the second index for each lat and lon
      real, optional, intent(in) :: lon(npts) ! array of longitudes in radians
      real, optional, intent(in) :: lat(npts) ! array of latitudes in radians
      real(kind=ESMF_KIND_R8), optional, intent(in) :: lonR8(npts) ! array of longitudes in radians
      real(kind=ESMF_KIND_R8), optional, intent(in) :: latR8(npts) ! array of latitudes in radians
      type(ESMF_Grid), optional, intent(inout) :: grid ! ESMF grid
      integer, optional, intent(out) :: rc ! return code

      integer :: status
      integer :: dims(3), im_world, jm_world
      real(kind=ESMF_KIND_R8), allocatable, dimension(:, :) :: xyz
      real(kind=ESMF_KIND_R8), allocatable, dimension(:) :: x, y, z
      real(kind=ESMF_KIND_R8), allocatable :: max_abs(:)
      real(kind=ESMF_KIND_R8) :: dalpha, shift0
      real(kind=ESMF_KIND_R8), allocatable :: lons(:), lats(:)
      ! sqrt(2.0d0), distance from center to the mid of an edge for a 2x2x2 cube
      real(kind=ESMF_KIND_R8), parameter :: sqr2 = 1.41421356237310d0
      ! asin(1.d0/sqrt(3.d0)),  angle between the two lines(center to the mid and center to the end of an edge)
      real(kind=ESMF_KIND_R8), parameter :: alpha = 0.615479708670387d0
      ! MAPL_PI_R8/18, Japan Fuji mountain shift
      real(kind=ESMF_KIND_R8), parameter :: shift = 0.174532925199433d0
      logical :: good_grid, stretched

      if (.not.present(grid)) then
         _FAIL("need a cubed-sphere grid")
      end if
      call MAPL_GridGet(grid, globalCellCountPerDim=dims, _RC)
      im_world = dims(1)
      jm_world = dims(2)
      _ASSERT(im_world * 6 == jm_world, "It only works for cubed-sphere grid")

      allocate(lons(npts), lats(npts))

      call reverse_schmidt( &
           grid, stretched, npts, &
           lon=lon, lat=lat, lonR8=lonR8, latR8=latR8, lonRe=lons, latRe=lats, _RC)

      dalpha = 2.0d0 * alpha / im_world

      ! make sure the grid can be used in this subroutine
      good_grid = grid_is_ok(grid)
      if (.not.good_grid) then
         _FAIL("get_global_horz_ij_index cannot handle this grid")
      end if
      ! Return if no local points
      _RETURN_IF(npts == 0)

      ! shift the grid away from Japan Fuji Mt.
      shift0 = shift
      if (stretched) shift0 = 0
      lons = lons + shift0

      ! get xyz from sphere surface
      allocate(xyz(3, npts), max_abs(npts))
      xyz(1, :) = cos(lats) * cos(lons)
      xyz(2, :) = cos(lats) * sin(lons)
      xyz(3, :) = sin(lats)

      ! project onto 2x2x2 cube
      max_abs = maxval(abs(xyz), dim=1)

      xyz(1, :) = xyz(1, :) / max_abs
      xyz(2, :) = xyz(2, :) / max_abs
      xyz(3, :) = xyz(3, :) / max_abs

      x = xyz(1, :)
      y = xyz(2, :)
      z = xyz(3, :)

      ii = -1
      jj = -1

      ! The edge points are assigned in the order of face 1,2,3,4,5,6
      call calculate(x, y, z, ii, jj)

      _RETURN(_SUCCESS)

   contains

      elemental subroutine calculate(x, y, z, i, j)
         real(kind=ESMF_KIND_R8), intent(in) :: x
         real(kind=ESMF_KIND_R8), intent(in) :: y
         real(kind=ESMF_KIND_R8), intent(in) :: z
         integer, intent(out) :: i, j
         real :: tolerance

         tolerance = epsilon(1.0d0)

         ! face = 1
         if (abs(x - 1.0d0) <= tolerance) then
            call angle_to_index(y, z, i, j)
            ! face = 2
         elseif (abs(y - 1.0d0) <= tolerance) then
            call angle_to_index(-x, z, i, j)
            j = j + im_world
            ! face = 3
         elseif (abs(z - 1.0d0) <= tolerance) then
            call angle_to_index(-x, -y, i, j)
            j = j + im_world * 2
            ! face = 4
         elseif (abs(x + 1.0d0) <= tolerance) then
            call angle_to_index(-z, -y, i, j)
            j = j + im_world * 3
            ! face = 5
         elseif (abs(y + 1.0d0) <= tolerance) then
            call angle_to_index(-z, x, i, j)
            j = j + im_world * 4
            ! face = 6
         elseif (abs(z + 1.0d0) <= tolerance) then
            call angle_to_index(y, x, i, j)
            j = j + im_world * 5
         end if

         if (i == 0) i = 1
         if (i == im_world + 1) i = im_world
      end subroutine calculate

      elemental subroutine angle_to_index(xval, yval, i, j)
         real(kind=ESMF_KIND_R8), intent(in) :: xval
         real(kind=ESMF_KIND_R8), intent(in) :: yval
         integer, intent(out) :: i, j
         i = ceiling((atan(xval / sqr2) + alpha) / dalpha)
         j = ceiling((atan(yval / sqr2) + alpha) / dalpha)
         if (j == 0) j = 1
         if (j == im_world + 1) j = im_world
      end subroutine angle_to_index

      function grid_is_ok(grid) result(OK)
         type(ESMF_Grid), intent(inout) :: grid
         logical :: OK
         integer :: i1, i2, j1, j2, j
         real(kind=ESMF_KIND_R8), allocatable :: corner_lons(:, :), corner_lats(:, :)
         real(kind=ESMF_KIND_R8), allocatable :: lonRe(:), latRe(:)
         real(kind=ESMF_KIND_R8), allocatable :: accurate_lat(:), accurate_lon(:)
         real(kind=ESMF_KIND_R8) :: shift0
         real :: tolerance
         integer :: local_dims(3)

         tolerance = epsilon(1.0)
         call MAPL_GridGetInterior(grid, i1, i2, j1, j2)
         call MAPL_GridGet(grid, localCellCountPerDim=local_dims, _RC)
         OK = .true.
         ! check the edge of face 1 along longitude
         ! call ESMF_GridGetCoord(grid,localDE=0,coordDim=1,staggerloc=ESMF_STAGGERLOC_CORNER,farrayPtr=corner_lons,_RC)
         ! call ESMF_GridGetCoord(grid,localDE=0,coordDim=2,staggerloc=ESMF_STAGGERLOC_CORNER,farrayPtr=corner_lats,_RC)
         allocate(corner_lons(local_dims(1) + 1, local_dims(2) + 1))
         allocate(corner_lats(local_dims(1) + 1, local_dims(2) + 1))
         call MAPL_GridGetCorners(grid, corner_lons, corner_lats, _RC)

         if (i1 == 1 .and. j1 == 1) then
            allocate(lonRe(local_dims(2)), latRe(local_dims(2)))
            call reverse_schmidt( &
                 grid, stretched, local_dims(2), &
                 lonR8=corner_lons(1, 1:local_dims(2)), latR8=corner_lats(1, 1:local_dims(2)), &
                 lonRe=lonRe, latRe=latRe, _RC)
            allocate(accurate_lon(local_dims(2)), accurate_lat(local_dims(2)))
            shift0 = shift
            if (stretched) shift0 = 0
            accurate_lon = 1.750d0 * MAPL_PI_R8 - shift0
            accurate_lat = [(-alpha + (j - 1) * dalpha, j = j1, j2)]
            if (any(abs(accurate_lon - lonRe) > 2.0 * tolerance) .or. &
                 any(abs(accurate_lat - latRe) > 2.0 * tolerance)) then
               print*, "Error: It could be "
               print*, "  1) grid may not have pi/18 Japan mountain shift"
               print*, "  2) grid is NOT gnomonic_ed;"
               print*, "  3) lats lons from MAPL_GridGetCorners are NOT accurate (single precision from ESMF)"
               print*, "  4) strtech grid rotates north pole"
               OK = .false.
               return
            end if
         end if
      end function grid_is_ok

   end subroutine get_global_horz_ij_index

   subroutine reverse_schmidt(grid, stretched, npts, lon, lat, lonR8, latR8, lonRe, latRe, rc)
      type(ESMF_Grid), intent(inout) :: grid
      logical, intent(out) :: stretched
      integer, intent(in) :: npts ! number of points in lat and lon arrays
      real, optional, intent(in) :: lon(npts) ! array of longitudes in radians
      real, optional, intent(in) :: lat(npts) ! array of latitudes in radians
      real(kind=ESMF_KIND_R8), optional, intent(in) :: lonR8(npts) ! array of longitudes in radians
      real(kind=ESMF_KIND_R8), optional, intent(in) :: latR8(npts)
      real(kind=ESMF_KIND_R8), optional, intent(out) :: lonRe(npts)
      real(kind=ESMF_KIND_R8), optional, intent(out) :: latRe(npts)
      integer, optional, intent(out) :: rc

      logical :: factorPresent, lonPresent, latPresent
      integer :: status
      real(kind=ESMF_KIND_R8) :: c2p1, c2m1, half_pi, two_pi, stretch_factor, target_lon, target_lat, &
           target_lon_degrees, target_lat_degrees
      real(kind=ESMF_KIND_R8), dimension(npts) :: x, y, z, Xx, Yy, Zz
      logical, dimension(npts) :: n_s
      type(ESMF_Info) :: infoh

      _RETURN_IF(npts == 0)

      call ESMF_InfoGetFromHost(grid, infoh, _RC)
      factorPresent = ESMF_InfoIsPresent(infoh, 'STRETCH_FACTOR', _RC)
      lonPresent = ESMF_InfoIsPresent(infoh, 'TARGET_LON', _RC)
      latPresent = ESMF_InfoIsPresent(infoh, 'TARGET_LAT', _RC)

      stretched = .false.
      if (factorPresent .and. lonPresent .and. latPresent) then
         stretched = .true.
      end if

      if (present(lonRe) .and. present(latRe)) then
         if (present(lonR8) .and. present(latR8)) then
            lonRe = lonR8
            latRe = latR8
         else if (present(lon) .and. present(lat)) then
            lonRe = lon
            latRe = lat
         else
            _FAIL("Need input to get the output lonRe, latRe")
         end if
      else
         _RETURN(_SUCCESS)
      end if

      if (.not.stretched) then
         _RETURN(_SUCCESS)
      end if

      call ESMF_InfoGet(infoh, 'STRETCH_FACTOR', value=stretch_factor, _RC)
      call ESMF_InfoGet(infoh, 'TARGET_LON', value=target_lon_degrees, _RC)
      call ESMF_InfoGet(infoh, 'TARGET_LAT', value=target_lat_degrees, _RC)

      c2p1 = 1 + stretch_factor * stretch_factor
      c2m1 = 1 - stretch_factor * stretch_factor

      half_pi = MAPL_PI_R8 / 2
      two_pi = MAPL_PI_R8 * 2

      target_lon = target_lon_degrees * MAPL_DEGREES_TO_RADIANS_R8
      target_lat = target_lat_degrees * MAPL_DEGREES_TO_RADIANS_R8

      x = cos(latRe) * cos(lonRe - target_lon)
      y = cos(latRe) * sin(lonRe - target_lon)
      z = sin(latRe)

      Xx = sin(target_lat) * x - cos(target_lat) * z
      Yy = -y
      Zz = -cos(target_lat) * x - sin(target_lat) * z

      n_s = (1. - abs(Zz)) < 10**(-7)

      where (n_s)
         lonRe = 0.0d0
         latRe = half_pi * sign(1.0d0, Zz)
         elsewhere
         lonRe = atan2(Yy, Xx)
         latRe = asin(Zz)
      end where

      if (abs(c2m1) > 10**(-7)) then !# unstretch
         latRe = asin((c2m1 - c2p1 * sin(latRe)) / (c2m1 * sin(latRe) - c2p1))
      end if

      where (lonRe < 0)
         lonRe = lonRe + two_pi
         elsewhere (lonRe >= two_pi)
         lonRe = lonRe - two_pi
      end where

      _RETURN(_SUCCESS)
   end subroutine reverse_schmidt

end module GetHorzIJIndex
