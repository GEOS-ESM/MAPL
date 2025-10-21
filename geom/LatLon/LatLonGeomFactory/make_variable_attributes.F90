#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) make_variable_attributes_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_LatLonDecomposition
   use mapl3g_LatLonGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL2_StringStringMap
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none (type, external)


contains

   module function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
      type(StringStringMap) :: variable_attributes
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      variable_attributes = StringStringMap()

      _RETURN(_SUCCESS)
   end function make_variable_attributes


end submodule make_variable_attributes_smod
