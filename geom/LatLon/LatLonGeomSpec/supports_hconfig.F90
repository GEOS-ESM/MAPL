#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) supports_hconfig_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   
contains

   logical module function supports_hconfig_(this, hconfig, rc) result(supports)
      class(LatLonGeomSpec), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      character(:), allocatable :: geom_class

      ! Mandatory entry: "class: latlon"
      supports = ESMF_HConfigIsDefined(hconfig, keystring='class', _RC)
      _RETURN_UNLESS(supports)

      geom_class = ESMF_HConfigAsString(hconfig, keyString='class', _RC)
      supports = (geom_class == 'latlon')
      _RETURN_UNLESS(supports)
      
      supports = lon_axis%supports(hconfig, _RC)
      _RETURN_UNLESS(supports)

      supports = lat_axis%supports(hconfig, _RC)
      _RETURN_UNLESS(supports)

      _RETURN(_SUCCESS)
   end function supports_hconfig_

end submodule supports_hconfig_smod
