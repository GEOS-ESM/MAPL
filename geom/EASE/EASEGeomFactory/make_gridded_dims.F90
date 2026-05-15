#include "MAPL.h"

submodule (mapl3g_EASEGeomFactory) make_gridded_dims_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEGeomSpec
   use mapl_ErrorHandlingMod
   use gFTL2_StringVector
   implicit none (type, external)

contains

   module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(EASEGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      gridded_dims = StringVector()
      select type (geom_spec)
      type is (EASEGeomSpec)
         call gridded_dims%push_back('lon')
         call gridded_dims%push_back('lat')
      class default
         _FAIL('make_gridded_dims: geom_spec is not of dynamic type EASEGeomSpec')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_gridded_dims

end submodule make_gridded_dims_smod
