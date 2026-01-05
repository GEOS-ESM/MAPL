#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) make_LatLonGeomSpec_from_metadata_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   
contains

   ! File metadata section

   ! Unfortunately, we cannot quite compute each axis (lat - lon) independently,
   ! as the optimal decomposition depends on the ratio of the extens along each
   ! dimension.
   module function make_LatLonGeomSpec_from_metadata(file_metadata, rc) result(spec)
      type(LatLonGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(LatLonDecomposition) :: decomposition

      lon_axis = make_LonAxis(file_metadata, _RC)
      lat_axis = make_LatAxis(file_metadata, _RC)

      associate (im_world => lon_axis%get_extent(), jm_world => lat_axis%get_extent())
        decomposition = make_LatLonDecomposition([im_world, jm_world], _RC)
      end associate
      spec = LatLonGeomSpec(lon_axis, lat_axis, decomposition)
      
      _RETURN(_SUCCESS)
   end function make_LatLonGeomSpec_from_metadata

end submodule make_LatLonGeomSpec_from_metadata_smod
