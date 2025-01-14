#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) supports_metadata_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   
contains

   logical module function supports_metadata_(this, file_metadata, rc) result(supports)
      class(LatLonGeomSpec), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis

      supports = .false.

      supports = lon_axis%supports(file_metadata, _RC)
      _RETURN_UNLESS(supports)

      supports = lat_axis%supports(file_metadata, _RC)
      _RETURN_UNLESS(supports)

      _RETURN(_SUCCESS)
   end function supports_metadata_

end submodule supports_metadata_smod
