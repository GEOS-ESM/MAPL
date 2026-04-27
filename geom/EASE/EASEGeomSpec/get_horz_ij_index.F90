#include "MAPL.h"

submodule (mapl3g_EASEGeomSpec) get_horz_ij_index_smod

   use mapl3g_EASEConversion
   use MAPL_Constants, only: MAPL_RADIANS_TO_DEGREES
   use mapl_ErrorHandling

   implicit none (type, external)

contains

   module subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
      class(EASEGeomSpec), intent(in)  :: this
      real(kind=R4),       intent(in)  :: lon(:)
      real(kind=R4),       intent(in)  :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional,   intent(out) :: rc

      integer :: status
      real(kind=R8), allocatable :: lon_r8(:), lat_r8(:)

      _ASSERT(size(lat) == size(lon), 'lon/lat size mismatch')
      lon_r8 = real(lon, kind=R8)
      lat_r8 = real(lat, kind=R8)
      call get_horz_ij_index_impl_(this, lon_r8, lat_r8, ii, jj, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r4

   module subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
      class(EASEGeomSpec), intent(in)  :: this
      real(kind=R8),       intent(in)  :: lon(:)
      real(kind=R8),       intent(in)  :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional,   intent(out) :: rc

      integer :: status

      _ASSERT(size(lat) == size(lon), 'lon/lat size mismatch')
      call get_horz_ij_index_impl_(this, lon, lat, ii, jj, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r8

   subroutine get_horz_ij_index_impl_(this, lon, lat, ii, jj, rc)
      class(EASEGeomSpec), intent(in)  :: this
      real(kind=R8),       intent(in)  :: lon(:)
      real(kind=R8),       intent(in)  :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional,   intent(out) :: rc

      integer :: k, npts, status
      real :: r, s, lat_deg, lon_deg

      npts = size(lon)
      _ASSERT(size(lat) == npts, 'lon/lat size mismatch')

      allocate(ii(npts), jj(npts))
      ii = -1
      jj = -1

      do k = 1, npts
         ! Inputs are in radians; convert to degrees for ease_convert
         lon_deg = real(lon(k) * MAPL_RADIANS_TO_DEGREES)
         lat_deg = real(lat(k) * MAPL_RADIANS_TO_DEGREES)

         ! Normalise longitude to [-180, 180)
         do while (lon_deg >  180.0) ; lon_deg = lon_deg - 360.0 ; end do
         do while (lon_deg < -180.0) ; lon_deg = lon_deg + 360.0 ; end do

         ! Use EASE forward conversion to get 0-based (r,s) indices
         call ease_convert(this%grid_name, lat_deg, lon_deg, r, s)

         ! Convert 0-based (r=col, s=row from N) to 1-based (ii=col, jj=row from S)
         ! r is 0-based column index; ii is 1-based
         ii(k) = nint(r) + 1
         ! s is 0-based row index from North; jj counts from South
         jj(k) = this%jm_world - nint(s)

         ! Clamp to valid range
         if (ii(k) < 1 .or. ii(k) > this%im_world) ii(k) = -1
         if (jj(k) < 1 .or. jj(k) > this%jm_world) jj(k) = -1
      end do

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_impl_

end submodule get_horz_ij_index_smod
