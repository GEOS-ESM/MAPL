#include "MAPL.h"

submodule (mapl3g_EASEGeomFactory) typesafe_make_file_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEGeomSpec
   use mapl3g_EASECoords
   use mapl_ErrorHandlingMod
   use pfio
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none (type, external)

contains

   module function typesafe_make_file_metadata(geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(EASEGeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: im, jm
      type(Variable) :: v
      real(kind=REAL64), allocatable :: lon_cen(:), lat_cen(:)

      call compute_lons(geom_spec, centers=lon_cen, _RC)
      call compute_lats(geom_spec, centers=lat_cen, _RC)

      im = geom_spec%get_im_world(_RC)
      jm = geom_spec%get_jm_world(_RC)

      call file_metadata%add_dimension('lon', im)
      call file_metadata%add_dimension('lat', jm)

      ! Longitude coordinate variable
      v = Variable(type=PFIO_REAL64, dimensions='lon', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(lon_cen))
      call file_metadata%add_variable('lon', v)

      ! Latitude coordinate variable
      v = Variable(type=PFIO_REAL64, dimensions='lat', chunksizes=chunksizes)
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(lat_cen))
      call file_metadata%add_variable('lat', v)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function typesafe_make_file_metadata

end submodule typesafe_make_file_metadata_smod
