#include "MAPL_ErrLog.h"
submodule (mapl3g_LatLonGeomFactory) typesafe_make_file_metadata_smod
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

   module function typesafe_make_file_metadata(geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(LatLonGeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(Variable) :: v

      lon_axis = geom_spec%get_lon_axis()
      lat_axis = geom_spec%get_lat_axis()
      
      call file_metadata%add_dimension('lon', lon_axis%get_extent())
      call file_metadata%add_dimension('lat', lat_axis%get_extent())

      ! Coordinate variables
      v = Variable(type=PFIO_REAL64, dimensions='lon', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(lon_axis%get_centers()))
      
      call file_metadata%add_variable('lon', v)

      v = Variable(type=PFIO_REAL64, dimensions='lat', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(lat_axis%get_centers()))
      call file_metadata%add_variable('lat', v)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function typesafe_make_file_metadata

end submodule typesafe_make_file_metadata_smod
