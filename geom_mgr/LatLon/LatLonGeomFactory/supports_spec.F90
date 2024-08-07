#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) supports_spec_smod
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

   logical module function supports_spec(this, geom_spec) result(supports)
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(LatLonGeomSpec) :: reference

      supports = same_type_as(geom_spec, reference)

   end function supports_spec

end submodule supports_spec_smod
