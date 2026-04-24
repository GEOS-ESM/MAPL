#include "MAPL.h"

submodule (mapl3g_XYGeomSpec) supports_hconfig_smod
   use mapl_ErrorHandlingMod
   use esmf
   implicit none

contains

   ! An hconfig block is an XY grid if it has:
   !   class: xy
   !   grid_file_name: <path>
   logical module function supports_hconfig_(this, hconfig, rc) result(supports)
      class(XYGeomSpec), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: geom_class

      supports = ESMF_HConfigIsDefined(hconfig, keystring='class', _RC)
      _RETURN_UNLESS(supports)

      geom_class = ESMF_HConfigAsString(hconfig, keyString='class', _RC)
      supports = (geom_class == 'xy')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_hconfig_

end submodule supports_hconfig_smod
