#include "MAPL_ErrLog.h"

submodule (mapl3g_CubedSphereGeomSpec) CubedSphereGeomSpec_get_horz_ij_index_smod

   use MAPL_Constants, only: MAPL_PI_R8
   use mapl_ErrorHandling

   implicit none(type, external)

contains

   module subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
      class(CubedSphereGeomSpec), intent(in) :: this
      real(kind=R4), intent(in) :: lon(:)
      real(kind=R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=R8), allocatable :: tmp_lons(:), tmp_lats(:)

      _ASSERT(size(lat) == size(lon), 'lon/lat size mismatch')
      tmp_lons = real(lon, kind=R8)
      tmp_lats = real(lat, kind=R8)

      call get_horz_ij_index_impl_(this, tmp_lons, tmp_lats, ii, jj, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r4

   module subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
      class(CubedSphereGeomSpec), intent(in) :: this
      real(kind=R8), intent(in) :: lon(:)
      real(kind=R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(size(lat) == size(lon), 'lon/lat size mismatch')

      call get_horz_ij_index_impl_(this, lon, lat, ii, jj, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r8

   subroutine get_horz_ij_index_impl_(this, lon, lat, ii, jj, rc)
      class(CubedSphereGeomSpec), intent(in) :: this
      real(kind=R8), intent(in) :: lon(:)
      real(kind=R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: npts, status
      real(kind=R8), allocatable :: xyz(:, :), max_abs(:), tmp_lons(:), tmp_lats(:)
      real(kind=R8) :: shift0
      logical :: stretched
      real(kind=R8), parameter :: shift = 0.174532925199433d0

      npts = size(lon)
      _ASSERT(size(lat) == npts, 'lon/lat size mismatch')
      tmp_lons = lon
      tmp_lats = lat
      _RETURN_UNLESS(npts > 0)

      allocate(ii(npts), jj(npts))

      call inverse_schmidt_(this%schmidt_parameters, npts, tmp_lons, tmp_lats, stretched)

      shift0 = shift
      if (stretched) shift0 = 0.0d0
      tmp_lons = tmp_lons + shift0

      allocate(xyz(3, npts), max_abs(npts))
      xyz(1, :) = cos(tmp_lats) * cos(tmp_lons)
      xyz(2, :) = cos(tmp_lats) * sin(tmp_lons)
      xyz(3, :) = sin(tmp_lats)

      max_abs = maxval(abs(xyz), dim=1)
      xyz = xyz / spread(max_abs, dim=1, ncopies=3)

      ii = -1
      jj = -1
      call calculate_(xyz(1, :), xyz(2, :), xyz(3, :), this%im_world, ii, jj)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_impl_

   elemental subroutine calculate_(x, y, z, im_world, i, j)
      real(kind=R8), intent(in) :: x
      real(kind=R8), intent(in) :: y
      real(kind=R8), intent(in) :: z
      integer, intent(in) :: im_world
      integer, intent(out) :: i
      integer, intent(out) :: j

      real(kind=R8) :: tolerance
      real(kind=R8) :: dalpha
      real(kind=R8), parameter :: sqr2 = 1.41421356237310d0
      real(kind=R8), parameter :: alpha = 0.615479708670387d0

      dalpha = 2.0d0 * alpha / im_world
      tolerance = epsilon(1.0d0)
      if (abs(x - 1.0d0) <= tolerance) then
         call angle_to_index_(y, z, i, j, im_world, dalpha, alpha, sqr2)
      elseif (abs(y - 1.0d0) <= tolerance) then
         call angle_to_index_(-x, z, i, j, im_world, dalpha, alpha, sqr2)
         j = j + im_world
      elseif (abs(z - 1.0d0) <= tolerance) then
         call angle_to_index_(-x, -y, i, j, im_world, dalpha, alpha, sqr2)
         j = j + im_world * 2
      elseif (abs(x + 1.0d0) <= tolerance) then
         call angle_to_index_(-z, -y, i, j, im_world, dalpha, alpha, sqr2)
         j = j + im_world * 3
      elseif (abs(y + 1.0d0) <= tolerance) then
         call angle_to_index_(-z, x, i, j, im_world, dalpha, alpha, sqr2)
         j = j + im_world * 4
      elseif (abs(z + 1.0d0) <= tolerance) then
         call angle_to_index_(y, x, i, j, im_world, dalpha, alpha, sqr2)
         j = j + im_world * 5
      end if

      if (i == 0) i = 1
      if (i == im_world + 1) i = im_world
   end subroutine calculate_

   elemental subroutine angle_to_index_(xval, yval, i, j, im_world, dalpha, alpha, sqr2)
      real(kind=R8), intent(in) :: xval
      real(kind=R8), intent(in) :: yval
      integer, intent(out) :: i
      integer, intent(out) :: j
      integer, intent(in) :: im_world
      real(kind=R8), intent(in) :: dalpha
      real(kind=R8), intent(in) :: alpha
      real(kind=R8), intent(in) :: sqr2

      i = ceiling((atan(xval / sqr2) + alpha) / dalpha)
      j = ceiling((atan(yval / sqr2) + alpha) / dalpha)
      if (j == 0) j = 1
      if (j == im_world + 1) j = im_world
   end subroutine angle_to_index_

   subroutine inverse_schmidt_(schmidt_parameters, npoints, lon_values, lat_values, is_stretched)
      type(ESMF_CubedSphereTransform_Args), intent(in) :: schmidt_parameters
      integer, intent(in) :: npoints
      real(kind=R8), intent(inout) :: lon_values(npoints)
      real(kind=R8), intent(inout) :: lat_values(npoints)
      logical, intent(out) :: is_stretched

      real(kind=R8) :: c2p1, c2m1, half_pi, two_pi
      real(kind=R8), dimension(npoints) :: x, y, z, xx, yy, zz
      logical, dimension(npoints) :: n_s

      is_stretched = (schmidt_parameters%target_lat /= undef_schmidt) .and. &
           (schmidt_parameters%target_lon /= undef_schmidt) .and. &
           (schmidt_parameters%stretch_factor /= undef_schmidt)
      if (.not. is_stretched) return

      c2p1 = 1.0d0 + schmidt_parameters%stretch_factor * schmidt_parameters%stretch_factor
      c2m1 = 1.0d0 - schmidt_parameters%stretch_factor * schmidt_parameters%stretch_factor
      half_pi = MAPL_PI_R8 / 2.0d0
      two_pi = MAPL_PI_R8 * 2.0d0

      x = cos(lat_values) * cos(lon_values - schmidt_parameters%target_lon)
      y = cos(lat_values) * sin(lon_values - schmidt_parameters%target_lon)
      z = sin(lat_values)

      xx = sin(schmidt_parameters%target_lat) * x - cos(schmidt_parameters%target_lat) * z
      yy = -y
      zz = -cos(schmidt_parameters%target_lat) * x - sin(schmidt_parameters%target_lat) * z

      n_s = (1.0d0 - abs(zz)) < 10.0d0**(-7)
      where (n_s)
         lon_values = 0.0d0
         lat_values = half_pi * sign(1.0d0, zz)
      elsewhere
         lon_values = atan2(yy, xx)
         lat_values = asin(zz)
      end where

      if (abs(c2m1) > 10.0d0**(-7)) then
         lat_values = asin((c2m1 - c2p1 * sin(lat_values)) / (c2m1 * sin(lat_values) - c2p1))
      end if

      where (lon_values < 0.0d0)
         lon_values = lon_values + two_pi
      elsewhere (lon_values >= two_pi)
         lon_values = lon_values - two_pi
      end where
   end subroutine inverse_schmidt_

end submodule CubedSphereGeomSpec_get_horz_ij_index_smod
