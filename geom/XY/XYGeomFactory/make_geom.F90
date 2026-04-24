#include "MAPL.h"

submodule (mapl3g_XYGeomFactory) make_geom_smod
   use mapl_ErrorHandlingMod
   use esmf
   implicit none (type, external)

contains

   module function make_geom(this, geom_spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(XYGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      select type (geom_spec)
      type is (XYGeomSpec)
         geom = typesafe_make_geom(geom_spec, _RC)
      class default
         _FAIL("geom_spec type not supported by XYGeomFactory")
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_geom

end submodule make_geom_smod
