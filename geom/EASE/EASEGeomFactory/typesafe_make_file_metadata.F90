#include "MAPL.h"

submodule (mapl3g_EASEGeomFactory) typesafe_make_file_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEGeomSpec
   use mapl_ErrorHandlingMod
   use pfio
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none (type, external)

contains

   module function typesafe_make_file_metadata(geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(EASEGeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      type(Variable) :: v

      call file_metadata%add_dimension('lon', geom_spec%get_im_world())
      call file_metadata%add_dimension('lat', geom_spec%get_jm_world())

      ! Longitude coordinate variable
      v = Variable(type=PFIO_REAL64, dimensions='lon', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(geom_spec%get_lon_centers()))
      call file_metadata%add_variable('lon', v)

      ! Latitude coordinate variable
      v = Variable(type=PFIO_REAL64, dimensions='lat', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(geom_spec%get_lat_centers()))
      call file_metadata%add_variable('lat', v)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function typesafe_make_file_metadata

end submodule typesafe_make_file_metadata_smod
