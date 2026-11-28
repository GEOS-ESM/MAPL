#include "MAPL.h"

submodule (mapl_LatLonGeomSpec_mod) make_variable_attributes_smod

   use mapl_ErrorHandling_mod
   use mapl_StringDictionary_mod

   implicit none (type, external)

contains

   module function make_variable_attributes(this, rc) &
        result(variable_attributes)
      type(StringDictionary) :: variable_attributes
      class(LatLonGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      variable_attributes = StringDictionary()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_variable_attributes

end submodule make_variable_attributes_smod
