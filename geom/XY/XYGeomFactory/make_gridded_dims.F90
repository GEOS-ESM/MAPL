#include "MAPL.h"

submodule (mapl3g_XYGeomFactory) make_gridded_dims_smod
   use mapl_ErrorHandlingMod
   use gftl2_StringVector
   implicit none (type, external)

contains

   module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(XYGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      gridded_dims = StringVector()
      select type (geom_spec)
      type is (XYGeomSpec)
         call gridded_dims%push_back('Xdim')
         call gridded_dims%push_back('Ydim')
      class default
         _FAIL('geom_spec is not of dynamic type XYGeomSpec.')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_gridded_dims

end submodule make_gridded_dims_smod
