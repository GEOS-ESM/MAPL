#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) supports_hconfig_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_LatLonDecomposition
   use mapl3g_LatLonGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none


contains

   logical module function supports_hconfig(this, hconfig, rc) result(supports)
      class(LatLonGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(LatLonGeomSpec) :: spec

      supports = spec%supports(hconfig, _RC)
      
      _RETURN(_SUCCESS)
   end function supports_hconfig

end submodule supports_hconfig_smod
