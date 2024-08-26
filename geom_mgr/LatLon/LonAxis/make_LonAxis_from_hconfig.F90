#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) make_LonAxis_from_hconfig_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   module function make_LonAxis_from_hconfig(hconfig, rc) result(axis)
      type(LonAxis) :: axis
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: im_world
      real(kind=R8), allocatable :: centers(:), corners(:)
      type(AxisRanges) :: ranges
      logical :: found

      !call MAPL_HConfigGet(hconfig, 'im_world', im_world, found=found, _RC)
      im_world = ESMF_HConfigAsI4(hconfig, keyString='im_world', asOkay=found, _RC)
      _ASSERT(found, '"im_world" not found.')
      _ASSERT(im_world > 0, "Config parameter 'im_world' must be greater than 0.")

      ranges = get_lon_range(hconfig, im_world, _RC)
      centers = MAPL_Range(ranges%center_min, ranges%center_max, im_world, _RC)
      corners = MAPL_Range(ranges%corner_min, ranges%corner_max, im_world+1, _RC)

      axis%CoordinateAxis = CoordinateAxis(centers, corners)

      _RETURN(_SUCCESS)
   end function make_LonAxis_from_hconfig

end submodule make_LonAxis_from_hconfig_smod

