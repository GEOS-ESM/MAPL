#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) make_geom_spec_from_metadata_smod
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
   implicit none


contains


   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LatLonGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_LatLonGeomSpec(file_metadata, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_metadata

end submodule make_geom_spec_from_metadata_smod
