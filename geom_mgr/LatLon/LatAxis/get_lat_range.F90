#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) get_lat_range_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none (type, external)

   integer, parameter :: R8 = ESMF_KIND_R8

contains

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

      pole = ESMF_HConfigAsString(hconfig, keyString='pole', _RC)
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

end submodule get_lat_range_smod

