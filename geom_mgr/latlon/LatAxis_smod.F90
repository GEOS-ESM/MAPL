#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) LatAxis_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none

   integer, parameter :: R8 = ESMF_KIND_R8

contains

   ! Constructor
   pure module function new_LatAxis(centers, corners) result(axis)
      type(LatAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)
      axis%CoordinateAxis = CoordinateAxis(centers, corners)
   end function new_LatAxis

   elemental logical module function equal_to(a, b)
      type(LatAxis), intent(in) :: a, b
      equal_to = (a%CoordinateAxis == b%CoordinateAxis)
   end function equal_to

   elemental logical module function not_equal_to(a, b)
      type(LatAxis), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

   logical module function supports_hconfig(hconfig, rc) result(supports)
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_jm_world
      logical :: has_lat_range
      logical :: has_pole
      supports = .true.

      has_jm_world = ESMF_HConfigIsDefined(hconfig, keystring='jm_world', _RC)
      _RETURN_UNLESS(has_jm_world)

      has_lat_range = ESMF_HConfigIsDefined(hconfig, keystring='lat_range', _RC)
      has_pole = ESMF_HConfigIsDefined(hconfig, keystring='pole', _RC)
      _RETURN_UNLESS(has_lat_range .neqv. has_pole)
      supports = .true.

      _RETURN(_SUCCESS)
   end function supports_hconfig


   logical module function supports_metadata(file_metadata, rc) result(supports)
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: dim_name

      supports = .true.
      dim_name = get_dim_name(file_metadata, units='degrees_north', _RC)

      supports = (dim_name /= '')
      _RETURN(_SUCCESS)
   end function supports_metadata

   

   ! static factory methods
   module function make_LatAxis_from_hconfig(hconfig, rc) result(axis)
      type(LatAxis) :: axis
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: jm_world
      real(kind=R8), allocatable :: centers(:), corners(:)
      type(AxisRanges) :: ranges
      logical :: found

      jm_world = ESMF_HConfigAsI4(hconfig, keyString='jm_world', asOkay=found, _RC)
      _ASSERT(found, '"jm_world" not found.')
!      call MAPL_HConfigGet(hconfig, 'jm_world', jm_world, _RC)
      _ASSERT(jm_world > 0, 'jm_world must be greater than 1')

      ranges = get_lat_range(hconfig, jm_world, _RC)
      centers = MAPL_Range(ranges%center_min, ranges%center_max, jm_world, _RC)

      corners = MAPL_Range(ranges%corner_min, ranges%corner_max, jm_world+1, _RC)
      ! IMPORTANT: this fix must be _after the call to MAPL_Range.
      if (corners(1) < -90.d0) corners(1) = -90.0d0
      if (corners(jm_world+1) > 90.d0) corners(jm_world+1) = 90.0d0

      axis%CoordinateAxis = CoordinateAxis(centers, corners)

      _RETURN(_SUCCESS)
   end function make_LatAxis_from_hconfig

   module function make_lataxis_from_metadata(file_metadata, rc) result(axis)
      type(LatAxis) :: axis
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      real(kind=R8), allocatable :: centers(:)
      real(kind=R8), allocatable :: corners(:)
      integer :: jm_world
      integer :: status
      character(:), allocatable :: dim_name

      dim_name = get_dim_name(file_metadata, units='degrees north', _RC)
      centers = get_coordinates(file_metadata, dim_name, _RC)
      jm_world = size(centers)
      call fix_bad_pole(centers)
      corners = get_lat_corners(centers)
      ! fix corners
      if (corners(1) < -90) corners(1) = -90
      if (corners(jm_world+1) > 90) corners(jm_world+1) = 90

      axis = LatAxis(centers, corners)

      _RETURN(_SUCCESS)
   end function make_lataxis_from_metadata

   module function get_lat_range(hconfig, jm_world, rc) result(ranges)
      type(AxisRanges) :: ranges
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: jm_world
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=R8) :: delta
      character(:), allocatable :: pole
      real, allocatable :: t_range(:)
      logical :: has_range
      logical :: has_pole

      has_range = ESMF_HConfigIsDefined(hconfig, keystring='lat_range', _RC)
      has_pole = ESMF_HConfigIsDefined(hconfig, keystring='pole', _RC)
      _ASSERT(has_range .neqv. has_pole, 'Exactly one of lon_range or pole must be defined in hconfig')

      if (has_range) then ! is_regional
         t_range = ESMF_HConfigAsR4Seq(hconfig, keyString='lat_range', _RC)
!         call MAPL_HConfigGet(hconfig, 'lat_range', t_range, _RC)
         _ASSERT(size(t_range) == 2, 'illegal size of lon_range')
         _ASSERT(range(1) < range(2), 'illegal lat_range')
         delta = (range(2) - range(1)) / jm_world
         ! t_range is corners; need centers
         ranges%center_min = t_range(1) + delta/2
         ranges%center_max = t_range(2) - delta/2
         ranges%corner_min = t_range(1)
         ranges%corner_max = t_range(2)
         _RETURN(_SUCCESS)
      end if

      pole = ESMF_HConfigAsString(hconfig, 'pole', _RC)
!      call MAPL_HConfigGet(hconfig, 'pole', pole, _RC)
      select case (pole)
      case ('PE')
         delta = 180.d0 / jm_world
         ranges%center_min = -90 + delta/2
         ranges%center_max = +90 - delta/2
         ranges%corner_min = -90
         ranges%corner_max = +90
      case ('PC')
         delta = 180.d0 / (jm_world-1)
         ranges%center_min = -90
         ranges%center_max = +90
         ranges%corner_min = -90 - delta/2
         ranges%corner_max = +90 + delta/2
      case default
         _FAIL("Illegal value for pole: "//pole)
      end select

      _RETURN(_SUCCESS)
   end function get_lat_range

   module function get_lat_corners(centers) result(corners)
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), allocatable :: corners(:)

      associate (jm => size(centers))
        allocate(corners(jm+1))
         corners(1) = centers(1) - (centers(2)-centers(1))/2
         corners(2:jm) = (centers(1:jm-1) + centers(2:jm))/2
         corners(jm+1) = centers(jm) + (centers(jm)-centers(jm-1))/2
      end associate
   end function get_lat_corners

   ! Magic code from ancient times.
   ! Do not touch unless you understand ...
   module subroutine fix_bad_pole(centers)
      real(kind=R8), intent(inout) :: centers(:)

      integer :: n
      real(kind=R8) :: d_lat, extrap_lat
      real, parameter :: tol = 1.0e-5

      if (size(centers) < 4) return ! insufficient data

      ! Check: is this a "mis-specified" pole-centered grid?
      ! Assume lbound=1 and ubound=size for now

      n = size(centers)
      d_lat = (centers(n-1) - centers(2)) / (n - 3)

      ! Check: is this a regular grid (i.e. constant spacing away from the poles)?
      if (any(((centers(2:n-1) - centers(1:n-2)) - d_lat) < tol*d_lat)) return

      ! Should the southernmost point actually be at the pole?
      extrap_lat = centers(2) - d_lat
      if (extrap_lat <= ((d_lat/20.0)-90.0)) then
         centers(1) = -90.0
      end if

      ! Should the northernmost point actually be at the pole?
      extrap_lat = centers(n-1) + d_lat
      if (extrap_lat >= (90.0-(d_lat/20.0))) then
         centers(n) =  90.0
      end if

   end subroutine fix_bad_pole

end submodule LatAxis_smod

