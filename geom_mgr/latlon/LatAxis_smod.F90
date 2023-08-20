#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) LatAxis_smod
   use mapl_RangeMod
   use mapl3g_HConfigUtils
   use mapl_ErrorHandling
   implicit none

   integer, parameter :: R8 = ESMF_KIND_R8

contains

   ! Constructor
   module function new_LatAxis(centers, corners) result(axis)
      type(LatAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)
      axis%CoordinateAxis = CoordinateAxis(centers, corners)
   end function new_LatAxis

   ! static factory methods
   module function make_LatAxis_from_hconfig(hconfig, rc) result(axis)
      type(LatAxis) :: axis
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: jm_world
      real(kind=R8), allocatable :: centers(:), corners(:)
      type(AxisRanges) :: ranges

      call MAPL_GetResource(jm_world, hconfig, 'jm_world', _RC)
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
         call MAPL_GetResource(t_range, hconfig, 'lat_range', _RC)
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

      call MAPL_GetResource(pole, hconfig, 'pole', _RC)
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

   elemental logical module function equal_to(a, b)
      type(LatAxis), intent(in) :: a, b
      equal_to = (a%CoordinateAxis == b%CoordinateAxis)
   end function equal_to

   elemental logical module function not_equal_to(a, b)
      type(LatAxis), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

end submodule LatAxis_smod

