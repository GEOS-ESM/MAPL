#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) get_horz_ij_index_smod

   use MAPL_Constants, only: MAPL_RADIANS_TO_DEGREES
   use mapl_ErrorHandling

   implicit none (type, external)

contains

   module subroutine get_horz_ij_index(this, ii, jj, lon, lat, lonR8, latR8, rc)

      class(LatLonGeomSpec), intent(in) :: this
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      real, optional, intent(in) :: lon(:)
      real, optional, intent(in) :: lat(:)
      real(kind=R8), optional, intent(in) :: lonR8(:)
      real(kind=R8), optional, intent(in) :: latR8(:)
      integer, optional, intent(out) :: rc

      integer :: i, npts, status
      real(kind=R8), allocatable :: lon_corners(:), lat_corners(:)
      real(kind=R8) :: lon_value, lat_value

      if (present(lonR8) .and. present(latR8)) then
         npts = size(lonR8)
         _ASSERT(size(latR8) == npts, 'lonR8/latR8 size mismatch')
      else if (present(lon) .and. present(lat)) then
         npts = size(lon)
         _ASSERT(size(lat) == npts, 'lon/lat size mismatch')
      else
         _FAIL('Need either lon/lat or lonR8/latR8 inputs')
      end if

      allocate(ii(npts), jj(npts))

      lon_corners = this%lon_axis%get_corners()
      lat_corners = this%lat_axis%get_corners()

      do i = 1, npts
         if (present(lonR8) .and. present(latR8)) then
            lon_value = lonR8(i) * MAPL_RADIANS_TO_DEGREES
            lat_value = latR8(i) * MAPL_RADIANS_TO_DEGREES
         else if (present(lon) .and. present(lat)) then
            lon_value = real(lon(i), kind=R8) * MAPL_RADIANS_TO_DEGREES
            lat_value = real(lat(i), kind=R8) * MAPL_RADIANS_TO_DEGREES
         else
            _FAIL('Need either lon/lat or lonR8/latR8 inputs')
         end if

         if (lon_value > 180.0d0) lon_value = lon_value - 360.0d0

         ii(i) = search_axis(lon_corners, lon_value, this%lon_axis%is_periodic())
         jj(i) = search_axis(lat_corners, lat_value, this%lat_axis%is_periodic())
      end do

      _RETURN(_SUCCESS)

   contains

      pure integer function search_axis(corners, value_in, periodic) result(idx)
         real(kind=R8), intent(in) :: corners(:)
         real(kind=R8), intent(in) :: value_in
         logical, intent(in) :: periodic

         integer :: lower, upper, middle, ncells
         logical :: ascending
         real(kind=R8) :: value, span

         idx = -1
         ncells = size(corners) - 1
         if (ncells <= 0) return

         ascending = corners(ncells + 1) >= corners(1)
         value = value_in

         if (periodic) then
            span = corners(ncells + 1) - corners(1)
            if (span <= 0.0d0) return
            do while (value < corners(1))
               value = value + span
            end do
            do while (value >= corners(ncells + 1))
               value = value - span
            end do
         else
            if (ascending) then
               if (value < corners(1) .or. value > corners(ncells + 1)) return
               if (value == corners(ncells + 1)) then
                  idx = ncells
                  return
               end if
            else
               if (value > corners(1) .or. value < corners(ncells + 1)) return
               if (value == corners(ncells + 1)) then
                  idx = ncells
                  return
               end if
            end if
         end if

         lower = 1
         upper = ncells + 1
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
      end function search_axis

   end subroutine get_horz_ij_index

end submodule get_horz_ij_index_smod