#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) supports_hconfig_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none (type, external)

   integer, parameter :: R8 = ESMF_KIND_R8

contains

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

end submodule supports_hconfig_smod

