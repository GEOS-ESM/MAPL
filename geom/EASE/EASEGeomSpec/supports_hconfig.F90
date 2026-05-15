#include "MAPL.h"

submodule (mapl3g_EASEGeomSpec) supports_hconfig_smod
   use mapl3g_GeomSpec
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)

contains

   ! An HConfig block is an EASE geom if it contains:
   !   class: ease
   ! and a grid name key:
   !   grid_name: EASEv2_M09   (or similar)
   logical module function supports_hconfig_(this, hconfig, rc) result(supports)
      class(EASEGeomSpec), intent(in) :: this
      type(ESMF_HConfig),  intent(in) :: hconfig
      integer, optional,   intent(out) :: rc

      integer :: status
      character(:), allocatable :: geom_class

      supports = ESMF_HConfigIsDefined(hconfig, keystring='class', _RC)
      _RETURN_UNLESS(supports)

      geom_class = ESMF_HConfigAsString(hconfig, keyString='class', _RC)
      supports = (geom_class == 'ease')
      _RETURN_UNLESS(supports)

      ! Must also have a grid_name key
      supports = ESMF_HConfigIsDefined(hconfig, keystring='grid_name', _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_hconfig_

end submodule supports_hconfig_smod
