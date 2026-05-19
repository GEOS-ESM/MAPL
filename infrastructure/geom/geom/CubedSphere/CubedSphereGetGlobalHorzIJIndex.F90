! `CubedSphereGetGlobalHorzIJIndex` -- Get global IJ indexes on a cubed-sphere ESMF grid
!
! For a set of longitudes and latitudes in radians this routine returns the global
! (not PE-local) IJ indices on a cubed-sphere grid. Returns -1 for any point that
! cannot be located. Handles stretched (Schmidt-transformed) grids.
!
! This logic was moved from MAPL_Base (MAPL_GetGlobalHorzIJIndex /
! MAPL_Reverse_Schmidt) into geom/CubedSphere as part of the MAPL v3 cleanup
! (#4837).  No public API is exposed yet; the module is compiled to preserve the
! logic until a proper GeomSpec method is added for Gigatraj and similar callers.

#include "MAPL.h"

module mapl3g_CubedSphereGetGlobalHorzIJIndex

  use ESMF,             only: ESMF_Grid, ESMF_Info, ESMF_KIND_R8, &
                               ESMF_InfoGetFromHost, ESMF_InfoIsPresent, ESMF_InfoGet, &
                               ESMF_GridGet, ESMF_STAGGERLOC_CENTER, ESMF_MAXDIM
  use mapl3g_GridGet,   only: GridGet
  use MAPL_Constants,   only: MAPL_PI_R8, MAPL_DEGREES_TO_RADIANS_R8
  use MAPL_ErrorHandling

  implicit none(type, external)
  private

  ! No public symbols intentionally — logic preserved for future GeomSpec method.

contains

  ! ---------------------------------------------------------------------------
  ! reverse_schmidt_
  !
  ! Undo the Schmidt (stretch) transformation applied to a cubed-sphere grid.
  ! Reads STRETCH_FACTOR, TARGET_LON, TARGET_LAT from ESMF_Info on the grid.
  ! Sets `stretched` to .true. if the grid carries stretch parameters.
  ! On input lonRe/latRe should contain the raw lon/lat; on output they hold
  ! the un-stretched lon/lat.
  ! ---------------------------------------------------------------------------
  subroutine reverse_schmidt_(grid, stretched, npts, lon, lat, lonR8, latR8, lonRe, latRe, rc)
    type(ESMF_Grid),                        intent(inout) :: grid
    logical,                                intent(out)   :: stretched
    integer,                                intent(in)    :: npts
    real,              optional,            intent(in)    :: lon(npts)
    real,              optional,            intent(in)    :: lat(npts)
    real(ESMF_KIND_R8), optional,           intent(in)    :: lonR8(npts)
    real(ESMF_KIND_R8), optional,           intent(in)    :: latR8(npts)
    real(ESMF_KIND_R8), optional,           intent(out)   :: lonRe(npts)
    real(ESMF_KIND_R8), optional,           intent(out)   :: latRe(npts)
    integer,            optional,           intent(out)   :: rc

    logical :: factorPresent, lonPresent, latPresent
    integer :: status
    real(ESMF_KIND_R8) :: c2p1, c2m1, half_pi, two_pi
    real(ESMF_KIND_R8) :: stretch_factor, target_lon, target_lat
    real(ESMF_KIND_R8) :: target_lon_degrees, target_lat_degrees
    real(ESMF_KIND_R8), dimension(npts) :: x, y, z, Xx, Yy, Zz
    logical,            dimension(npts) :: n_s
    type(ESMF_Info) :: infoh

    _RETURN_IF(npts == 0)

    call ESMF_InfoGetFromHost(grid, infoh, _RC)
    factorPresent = ESMF_InfoIsPresent(infoh, 'STRETCH_FACTOR', _RC)
    lonPresent    = ESMF_InfoIsPresent(infoh, 'TARGET_LON',     _RC)
    latPresent    = ESMF_InfoIsPresent(infoh, 'TARGET_LAT',     _RC)

    stretched = (factorPresent .and. lonPresent .and. latPresent)

    if (present(lonRe) .and. present(latRe)) then
      if (present(lonR8) .and. present(latR8)) then
        lonRe = lonR8
        latRe = latR8
      else if (present(lon) .and. present(lat)) then
        lonRe = lon
        latRe = lat
      else
        _FAIL("Need input lon/lat to produce lonRe/latRe")
      end if
    else
      _RETURN(_SUCCESS)
    end if

    if (.not. stretched) then
      _RETURN(_SUCCESS)
    end if

    call ESMF_InfoGet(infoh, 'STRETCH_FACTOR', value=stretch_factor,       _RC)
    call ESMF_InfoGet(infoh, 'TARGET_LON',     value=target_lon_degrees,    _RC)
    call ESMF_InfoGet(infoh, 'TARGET_LAT',     value=target_lat_degrees,    _RC)

    c2p1 = 1.0d0 + stretch_factor*stretch_factor
    c2m1 = 1.0d0 - stretch_factor*stretch_factor

    half_pi = MAPL_PI_R8 / 2.0d0
    two_pi  = MAPL_PI_R8 * 2.0d0

    target_lon = target_lon_degrees * MAPL_DEGREES_TO_RADIANS_R8
    target_lat = target_lat_degrees * MAPL_DEGREES_TO_RADIANS_R8

    x = cos(latRe) * cos(lonRe - target_lon)
    y = cos(latRe) * sin(lonRe - target_lon)
    z = sin(latRe)

    Xx =  sin(target_lat)*x - cos(target_lat)*z
    Yy = -y
    Zz = -cos(target_lat)*x - sin(target_lat)*z

    n_s = (1.0d0 - abs(Zz)) < 1.0d-7

    where (n_s)
      lonRe = 0.0d0
      latRe = half_pi * sign(1.0d0, Zz)
    elsewhere
      lonRe = atan2(Yy, Xx)
      latRe = asin(Zz)
    end where

    if (abs(c2m1) > 1.0d-7) then
      latRe = asin((c2m1 - c2p1*sin(latRe)) / (c2m1*sin(latRe) - c2p1))
    end if

    where (lonRe < 0.0d0)
      lonRe = lonRe + two_pi
    elsewhere (lonRe >= two_pi)
      lonRe = lonRe - two_pi
    end where

    _RETURN(_SUCCESS)

  end subroutine reverse_schmidt_

  ! ---------------------------------------------------------------------------
  ! get_global_horz_ij_index_
  !
  ! Returns global IJ indices for arbitrary lon/lat points on a cubed-sphere
  ! ESMF grid. Returns -1 for unlocated points.
  ! ---------------------------------------------------------------------------
  subroutine get_global_horz_ij_index_(npts, II, JJ, lon, lat, lonR8, latR8, grid, rc)
    integer,                                intent(in)    :: npts
    integer,                                intent(inout) :: II(npts)
    integer,                                intent(inout) :: JJ(npts)
    real,              optional,            intent(in)    :: lon(npts)
    real,              optional,            intent(in)    :: lat(npts)
    real(ESMF_KIND_R8), optional,           intent(in)    :: lonR8(npts)
    real(ESMF_KIND_R8), optional,           intent(in)    :: latR8(npts)
    type(ESMF_Grid),   optional,            intent(inout) :: grid
    integer,           optional,            intent(out)   :: rc

    integer :: status, IM_WORLD, JM_WORLD
    integer :: mincounts(ESMF_MAXDIM), maxcounts(ESMF_MAXDIM)
    real(ESMF_KIND_R8), allocatable :: xyz(:,:), x(:), y(:), z(:), max_abs(:)
    real(ESMF_KIND_R8), allocatable :: lons(:), lats(:)
    real(ESMF_KIND_R8)              :: dalpha, shift0
    logical :: good_grid, stretched

    ! sqrt(2): distance from centre to mid-edge of 2x2x2 cube
    real(ESMF_KIND_R8), parameter :: sqr2  = 1.41421356237310d0
    ! asin(1/sqrt(3)): angle between centre-to-mid-edge and centre-to-corner
    real(ESMF_KIND_R8), parameter :: alpha = 0.615479708670387d0
    ! pi/18: Japan Fuji-mountain shift
    real(ESMF_KIND_R8), parameter :: shift = 0.174532925199433d0

    _ASSERT(present(grid), "need a cubed-sphere grid")
    call ESMF_GridGet(grid, tile=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
         minIndex=mincounts, maxIndex=maxcounts, _RC)
    IM_WORLD = maxcounts(1) - mincounts(1) + 1
    JM_WORLD = IM_WORLD * 6
    _ASSERT(IM_WORLD > 0, "only works for cubed-sphere grid")

    allocate(lons(npts), lats(npts))
    call reverse_schmidt_(grid, stretched, npts, lon=lon, lat=lat, lonR8=lonR8, latR8=latR8, lonRe=lons, latRe=lats, _RC)

    dalpha = 2.0d0 * alpha / IM_WORLD

    good_grid = grid_is_ok_(grid, stretched, IM_WORLD, dalpha, alpha, shift)
    _ASSERT(good_grid, "get_global_horz_ij_index_: grid failed validation")

    _RETURN_IF(npts == 0)

    shift0 = shift
    if (stretched) shift0 = 0.0d0
    lons = lons + shift0

    allocate(xyz(3, npts), max_abs(npts))
    xyz(1,:) = cos(lats) * cos(lons)
    xyz(2,:) = cos(lats) * sin(lons)
    xyz(3,:) = sin(lats)

    max_abs = maxval(abs(xyz), dim=1)
    xyz(1,:) = xyz(1,:) / max_abs
    xyz(2,:) = xyz(2,:) / max_abs
    xyz(3,:) = xyz(3,:) / max_abs

    x = xyz(1,:)
    y = xyz(2,:)
    z = xyz(3,:)

    II = -1
    JJ = -1
    call calculate_(x, y, z, II, JJ, IM_WORLD, alpha, dalpha, sqr2)

    _RETURN(_SUCCESS)

  end subroutine get_global_horz_ij_index_

  ! ---------------------------------------------------------------------------
  ! Internal helpers
  ! ---------------------------------------------------------------------------

  subroutine calculate_(x, y, z, II, JJ, IM_WORLD, alpha, dalpha, sqr2)
    real(ESMF_KIND_R8), intent(in)    :: x(:), y(:), z(:)
    integer,            intent(inout) :: II(:), JJ(:)
    integer,            intent(in)    :: IM_WORLD
    real(ESMF_KIND_R8), intent(in)    :: alpha, dalpha, sqr2
    integer :: n

    do n = 1, size(x)
      call calculate_one_(x(n), y(n), z(n), II(n), JJ(n), IM_WORLD, alpha, dalpha, sqr2)
    end do
  end subroutine calculate_

  subroutine calculate_one_(x, y, z, i, j, IM_WORLD, alpha, dalpha, sqr2)
    real(ESMF_KIND_R8), intent(in)  :: x, y, z
    integer,            intent(out) :: i, j
    integer,            intent(in)  :: IM_WORLD
    real(ESMF_KIND_R8), intent(in)  :: alpha, dalpha, sqr2
    real(ESMF_KIND_R8), parameter   :: tol = epsilon(1.0d0)

    i = -1; j = -1
    if      (abs(x - 1.0d0) <= tol) then
      call angle_to_index_(y,  z,  i, j, alpha, dalpha, sqr2, IM_WORLD)
    else if (abs(y - 1.0d0) <= tol) then
      call angle_to_index_(-x, z,  i, j, alpha, dalpha, sqr2, IM_WORLD)
      j = j + IM_WORLD
    else if (abs(z - 1.0d0) <= tol) then
      call angle_to_index_(-x, -y, i, j, alpha, dalpha, sqr2, IM_WORLD)
      j = j + IM_WORLD*2
    else if (abs(x + 1.0d0) <= tol) then
      call angle_to_index_(-z, -y, i, j, alpha, dalpha, sqr2, IM_WORLD)
      j = j + IM_WORLD*3
    else if (abs(y + 1.0d0) <= tol) then
      call angle_to_index_(-z,  x, i, j, alpha, dalpha, sqr2, IM_WORLD)
      j = j + IM_WORLD*4
    else if (abs(z + 1.0d0) <= tol) then
      call angle_to_index_(y,   x, i, j, alpha, dalpha, sqr2, IM_WORLD)
      j = j + IM_WORLD*5
    end if
    if (i == 0)          i = 1
    if (i == IM_WORLD+1) i = IM_WORLD
  end subroutine calculate_one_

  subroutine angle_to_index_(xval, yval, i, j, alpha, dalpha, sqr2, IM_WORLD)
    real(ESMF_KIND_R8), intent(in)  :: xval, yval, alpha, dalpha, sqr2
    integer,            intent(out) :: i, j
    integer,            intent(in)  :: IM_WORLD
    i = ceiling((atan(xval/sqr2) + alpha) / dalpha)
    j = ceiling((atan(yval/sqr2) + alpha) / dalpha)
    if (j == 0)          j = 1
    if (j == IM_WORLD+1) j = IM_WORLD
  end subroutine angle_to_index_

  logical function grid_is_ok_(grid, stretched, IM_WORLD, dalpha, alpha, shift) result(ok)
    type(ESMF_Grid),    intent(inout) :: grid
    logical,            intent(in)    :: stretched
    integer,            intent(in)    :: IM_WORLD
    real(ESMF_KIND_R8), intent(in)    :: dalpha, alpha, shift

    integer :: I1, I2, J1, J2, j, im, jm, status, rc
    integer, allocatable :: interior(:)
    real(ESMF_KIND_R8), allocatable :: corners(:,:,:)
    real(ESMF_KIND_R8), allocatable :: lonRe(:), latRe(:)
    real(ESMF_KIND_R8), allocatable :: accurate_lat(:), accurate_lon(:)
    real(ESMF_KIND_R8) :: shift0
    real :: tolerance
    logical :: dummy_stretched

    tolerance = epsilon(1.0)
    ok = .true.

    call GridGet(grid, interior=interior, corners=corners, im=im, jm=jm, _RC)
    I1 = interior(1); I2 = interior(2)
    J1 = interior(3); J2 = interior(4)

    if (I1 == 1 .and. J1 == 1) then
      allocate(lonRe(jm), latRe(jm))
      call reverse_schmidt_(grid, dummy_stretched, jm, &
                            lonR8=corners(1, 1:jm, 1), &
                            latR8=corners(1, 1:jm, 2), &
                            lonRe=lonRe, latRe=latRe, _RC)

      allocate(accurate_lon(jm), accurate_lat(jm))
      shift0 = shift
      if (stretched) shift0 = 0.0d0

      accurate_lon = 1.750d0*MAPL_PI_R8 - shift0
      accurate_lat = [(-alpha + (j-1)*dalpha, j = J1, J2)]

      if (any(abs(accurate_lon - lonRe) > 20.0*tolerance) .or. &
          any(abs(accurate_lat - latRe) > 20.0*tolerance)) then
        print*, "Error in grid_is_ok_: grid may not have pi/18 Japan mountain shift,"
        print*, "  may not be gnomonic_ed, or corners are not accurate enough."
        ok = .false.
      end if
    end if

  end function grid_is_ok_

end module mapl3g_CubedSphereGetGlobalHorzIJIndex
