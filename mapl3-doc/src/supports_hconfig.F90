#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) supports_hconfig_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   logical module function supports_hconfig(hconfig, rc) result(supports)
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_im_world
      logical :: has_lon_range
      logical :: has_dateline

      supports = .true.

      has_im_world = ESMF_HConfigIsDefined(hconfig, keystring='lon_range', _RC)
      _RETURN_UNLESS(has_im_world)

      has_lon_range = ESMF_HConfigIsDefined(hconfig, keystring='lon_range', _RC)
      has_dateline = ESMF_HConfigIsDefined(hconfig, keystring='dateline', _RC)
      _RETURN_UNLESS(has_lon_range .neqv. has_dateline)
      supports = .true.

      _RETURN(_SUCCESS)
   end function supports_hconfig

end submodule supports_hconfig_smod

