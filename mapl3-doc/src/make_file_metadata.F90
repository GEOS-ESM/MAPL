#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) make_file_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl3g_LatLonDecomposition
   use mapl3g_LatLonGeomSpec
   use mapl_MinMaxMod
   use mapl_ErrorHandlingMod
   use mapl_Constants
   use pFIO
   use gFTL2_StringVector
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none (type, external)


contains

   module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
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
   end function make_file_metadata

end submodule make_file_metadata_smod
