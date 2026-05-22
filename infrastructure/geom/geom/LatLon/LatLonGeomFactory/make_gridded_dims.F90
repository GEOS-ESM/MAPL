#include "MAPL.h"

submodule (mapl_LatLonGeomFactory_mod) make_gridded_dims_smod

   use mapl_GeomSpec_mod
   use mapl_LonAxis_mod
   use mapl_LatAxis_mod
   use mapl_LatLonDecomposition_mod
   use mapl_LatLonGeomSpec_mod
   use mapl_MinMax_mod
   use mapl_ErrorHandling_mod
   use mapl_Constants_mod
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer

   implicit none (type, external)

contains

   module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      gridded_dims = StringVector()
      select type (geom_spec)
      type is (LatLonGeomSpec)
         call gridded_dims%push_back('lon')
         call gridded_dims%push_back('lat')
      class default
         _FAIL('geom_spec is not of dynamic type LatLonGeomSpec.')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_gridded_dims


end submodule make_gridded_dims_smod
