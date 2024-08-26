#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) get_lon_range_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   module function get_lon_range(hconfig, im_world, rc) result(ranges)
      type(AxisRanges) :: ranges
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: im_world
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=R8) :: delta
      character(:), allocatable :: dateline
      real(kind=ESMF_KIND_R4), allocatable :: t_range(:)
      logical :: has_range
      logical :: has_dateline

      has_range = ESMF_HConfigIsDefined(hconfig, keystring='lon_range', _RC)
      has_dateline = ESMF_HConfigIsDefined(hconfig, keystring='dateline', _RC)
      _ASSERT(has_range .neqv. has_dateline, 'Exactly one of lon_range or dateline must be defined in hconfig')

      if (has_range) then ! is regional
         t_range = ESMF_HConfigAsR4Seq(hconfig, keyString='lon_range', _RC)
         _ASSERT(size(t_range) == 2, 'illegal size of lon_range')
         _ASSERT(t_range(1) < t_range(2), 'illegal lon_range')
         delta = (t_range(2) - t_range(1)) / im_world

         ranges%corner_min = t_range(1)
         ranges%corner_max = t_range(2)
         ranges%center_min = t_range(1) + delta/2
         ranges%center_max = t_range(2) - delta/2
         _RETURN(_SUCCESS)
      end if

      delta = 360.d0 / im_world
      dateline = ESMF_HConfigAsString(hconfig, keyString='dateline', _RC)
      select case (dateline)
      case ('DC')
         ranges%corner_min = -180.d0 - delta/2
         ranges%corner_max = +180.d0 - delta/2
         ranges%center_min = -180
         ranges%center_max = +180 - delta
      case ('DE')
         ranges%corner_min = -180
         ranges%corner_max = +180
         ranges%center_min = -180 + delta/2
         ranges%center_max = +180 - delta/2
      case ('GC')
         ranges%corner_min = -delta/2
         ranges%corner_max = 360 - delta/2
         ranges%center_min = 0
         ranges%center_max = 360 - delta
      case ('GE')
         ranges%corner_min = 0
         ranges%corner_max = 360 - delta
         ranges%center_min = delta/2
         ranges%center_max = 360 - delta/2
      case default
         _FAIL("Illegal value for dateline: "//dateline)
      end select

      _RETURN(_SUCCESS)
   end function get_lon_range

end submodule get_lon_range_smod

