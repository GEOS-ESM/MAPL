#include "MAPL.h"

submodule (mapl_LatLonGeomSpec_mod) get_horz_ij_index_smod

   use MAPL_Constants, only: MAPL_RADIANS_TO_DEGREES, MAPL_DEGREES_TO_RADIANS_R8
   use mapl_ErrorHandling_mod
   use ESMF, only: ESMF_GeomGet, ESMF_GeomType_Flag, ESMF_GEOMTYPE_GRID, ESMF_Grid
   use mapl_geom_api, only: mapl_GridGet

   implicit none (type, external)

contains

   module subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, geom, rc)

      class(LatLonGeomSpec), intent(in) :: this
      real(kind=R4), intent(in) :: lon(:)
      real(kind=R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=R8), allocatable :: lon_r8(:), lat_r8(:)
      real(kind=R8), allocatable :: lon_corners(:), lat_corners(:)

      _ASSERT(size(lat) == size(lon), 'lon/lat size mismatch')
      lon_r8 = real(lon, kind=R8)
      lat_r8 = real(lat, kind=R8)

      ! Clamp to the axis domain to absorb R4->R8 rounding near boundaries.
      ! A cell-centre coordinate at exactly +/-90 degrees in R4 may round to
      ! a value slightly outside the R8 corner after promotion, causing a
      ! false out-of-bounds result.  Corners are stored in degrees; convert
      ! to radians before clamping to match the units of lon_r8/lat_r8.
      lon_corners = this%lon_axis%get_corners() * MAPL_DEGREES_TO_RADIANS_R8
      lat_corners = this%lat_axis%get_corners() * MAPL_DEGREES_TO_RADIANS_R8
      lon_r8 = max(lon_corners(1), min(lon_corners(size(lon_corners)), lon_r8))
      lat_r8 = max(lat_corners(1), min(lat_corners(size(lat_corners)), lat_r8))

      call get_horz_ij_index_impl_(this, lon_r8, lat_r8, ii, jj, geom=geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r4

   module subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, geom, rc)

      class(LatLonGeomSpec), intent(in) :: this
      real(kind=R8), intent(in) :: lon(:)
      real(kind=R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(size(lat) == size(lon), 'lon/lat size mismatch')

       call get_horz_ij_index_impl_(this, lon, lat, ii, jj, geom=geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_horz_ij_index_r8

   subroutine get_horz_ij_index_impl_(this, lon, lat, ii, jj, geom, rc)

      class(LatLonGeomSpec), intent(in) :: this
      real(kind=R8), intent(in) :: lon(:)
      real(kind=R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: i, npts, status
      real(kind=R8), allocatable :: lon_corners(:), lat_corners(:)
      real(kind=R8) :: lon_value, lat_value
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid
      integer, allocatable :: interior(:)

      npts = size(lon)
      _ASSERT(size(lat) == npts, 'lon/lat size mismatch')

      allocate(ii(npts), jj(npts))

      lon_corners = this%lon_axis%get_corners()
      lat_corners = this%lat_axis%get_corners()

      do i = 1, npts
         lon_value = lon(i) * MAPL_RADIANS_TO_DEGREES
         lat_value = lat(i) * MAPL_RADIANS_TO_DEGREES

         if (lon_value > 180.0d0) lon_value = lon_value - 360.0d0

         ii(i) = search_axis_(lon_corners, lon_value, this%lon_axis%is_periodic())
         jj(i) = search_axis_(lat_corners, lat_value, this%lat_axis%is_periodic())
      end do

      if (present(geom)) then
         call ESMF_GeomGet(geom, geomtype=geomtype, rc=status)
         _VERIFY(status)
         _ASSERT(geomtype == ESMF_GEOMTYPE_GRID, 'local_indices=.true. is only supported for ESMF_Grid')
         call ESMF_GeomGet(geom, grid=grid, rc=status)
         _VERIFY(status)
         call mapl_GridGet(grid, interior=interior, _RC)
         where ( interior(1) <= ii .and. ii <= interior(2) .and. interior(3) <= jj .and. jj <= interior(4))
            ii = ii - interior(1) + 1
            jj = jj - interior(3) + 1
         elsewhere
            ii = -1
            jj = -1
         end where
      end if

      _RETURN(_SUCCESS)

   end subroutine get_horz_ij_index_impl_

   pure integer function search_axis_(corners, value_in, periodic) result(idx)
      real(kind=R8), intent(in) :: corners(:)
      real(kind=R8), intent(in) :: value_in
      logical, intent(in) :: periodic

      integer :: ncells
      logical :: ascending, ok
      real(kind=R8) :: value

      idx = -1
      ncells = size(corners) - 1
      if (ncells <= 0) return

      ascending = corners(ncells + 1) >= corners(1)
      value = value_in

      if (periodic) then
         call normalize_periodic_(corners, value, ok)
         if (.not. ok) return
      else if (.not. in_bounds_(corners, value, ascending)) then
         return
      else if (value == corners(ncells + 1)) then
         idx = ncells
         return
      end if

      idx = binary_search_(corners, value, ascending)
   end function search_axis_

   pure subroutine normalize_periodic_(corners, value, ok)
      real(kind=R8), intent(in) :: corners(:)
      real(kind=R8), intent(inout) :: value
      logical, intent(out) :: ok

      real(kind=R8) :: span

      span = corners(size(corners)) - corners(1)
      ok = span > 0.0d0
      if (.not. ok) return

      do while (value < corners(1))
         value = value + span
      end do
      do while (value >= corners(size(corners)))
         value = value - span
      end do
   end subroutine normalize_periodic_

   pure logical function in_bounds_(corners, value, ascending) result(is_in_bounds)
      real(kind=R8), intent(in) :: corners(:)
      real(kind=R8), intent(in) :: value
      logical, intent(in) :: ascending

      if (ascending) then
         is_in_bounds = value >= corners(1) .and. value <= corners(size(corners))
      else
         is_in_bounds = value <= corners(1) .and. value >= corners(size(corners))
      end if
   end function in_bounds_

   pure integer function binary_search_(corners, value, ascending) result(idx)
      real(kind=R8), intent(in) :: corners(:)
      real(kind=R8), intent(in) :: value
      logical, intent(in) :: ascending

      integer :: lower, upper, middle

      lower = 1
      upper = size(corners)
      do while (upper - lower > 1)
         middle = (lower + upper) / 2
         if (ascending) then
            if (value < corners(middle)) then
               upper = middle
            else
               lower = middle
            end if
         else
            if (value > corners(middle)) then
               upper = middle
            else
               lower = middle
            end if
         end if
      end do

      idx = lower
   end function binary_search_

end submodule get_horz_ij_index_smod
