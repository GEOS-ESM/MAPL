#include "MAPL.h"

submodule (mapl_LatLonGeomFactory_mod) make_file_metadata_smod

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

   module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
      use mapl_KeywordEnforcer_mod
      type(FileMetadata) :: file_metadata
      class(LatLonGeomFactory), intent(in) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      file_metadata = FileMetadata()

      select type (geom_spec)
      type is (LatLonGeomSpec)
         file_metadata = typesafe_make_file_metadata(geom_spec, chunksizes=chunksizes, _RC)
      class default
         _FAIL('geom_spec is not of dynamic type LatLonGeomSpec.')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
   end function make_file_metadata

end submodule make_file_metadata_smod
