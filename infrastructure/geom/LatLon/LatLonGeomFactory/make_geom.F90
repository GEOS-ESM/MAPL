#include "MAPL.h"

submodule (mapl_LatLonGeomFactory_mod) make_geom_smod

   use mapl_GeomSpec_mod
   use mapl_LonAxis_mod
   use mapl_LatAxis_mod
   use mapl_LatLonDecomposition_mod
   use mapl_LatLonGeomSpec_mod
   use mapl_MinMax_mod
   use mapl_ErrorHandling_mod
   use MAPL_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer

   implicit none (type, external)

contains

   module function make_geom(this, geom_spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      select type (geom_spec)
      type is (LatLonGeomSpec)
         geom = typesafe_make_geom(geom_spec, _RC)
      class default
         _FAIL("geom_spec type not supported")
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_geom

end submodule make_geom_smod
