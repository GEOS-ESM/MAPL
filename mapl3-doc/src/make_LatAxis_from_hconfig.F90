#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) make_LatAxis_from_hconfig_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none (type, external)

   integer, parameter :: R8 = ESMF_KIND_R8

contains

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

end submodule make_LatAxis_from_hconfig_smod

