#include "MAPL.h"

submodule (mapl_EASEGeomFactory_mod) make_variable_attributes_smod
   use mapl_GeomSpec_mod
   use mapl_EASEGeomSpec_mod
   use mapl_ErrorHandling_mod
   use mapl_StringDictionary_mod
   implicit none (type, external)

contains

   module function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
      type(StringDictionary) :: variable_attributes
      class(EASEGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      variable_attributes = StringDictionary()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(geom_spec)
   end function make_variable_attributes

end submodule make_variable_attributes_smod
