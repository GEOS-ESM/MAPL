#include "MAPL.h"

submodule (mapl3g_LatLonGeomSpec) supports_metadata_smod

   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
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
      character(:), allocatable :: lon_dim, lat_dim

      supports = .false.

      ! Require that both longitude and latitude axes are
      ! supported in the usual way.

      supports = lon_axis%supports(file_metadata, _RC)
      _RETURN_UNLESS(supports)

      supports = lat_axis%supports(file_metadata, _RC)
      _RETURN_UNLESS(supports)

      ! Distinguish regular LatLon grids from LocStreams. For
      ! LatLon we expect distinct latitude and longitude
      ! dimensions (e.g. lat x lon), whereas LocStreams share a
      ! single dimension for both coordinates. If both
      ! coordinates share the same dimension, consider this not
      ! a LatLon grid so that LocStream factories can claim it.

      lon_dim = get_dim_name(file_metadata, units='degrees_east', _RC)
      lat_dim = get_dim_name(file_metadata, units='degrees_north', _RC)

      supports = (lon_dim /= '' .and. lat_dim /= '' .and. lon_dim /= lat_dim)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_metadata_

end submodule supports_metadata_smod
