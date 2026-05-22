#include "MAPL.h"

submodule (mapl_LatLonGeomFactory_mod) make_variable_attributes_smod

   use mapl_GeomSpec_mod
   use mapl_LonAxis_mod
   use mapl_LatAxis_mod
   use mapl_LatLonDecomposition_mod
   use mapl_LatLonGeomSpec_mod
   use mapl_MinMax_mod
   use mapl_ErrorHandling_mod
   use mapl_Constants_mod
   use pFIO
   use mapl_StringDictionary_mod
   use esmf
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer

   implicit none (type, external)

contains

   module function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
      type(StringDictionary) :: variable_attributes
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      variable_attributes = StringDictionary()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(geom_spec)
   end function make_variable_attributes

end submodule make_variable_attributes_smod
