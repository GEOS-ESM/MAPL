#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) make_lataxis_from_metadata_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none (type, external)

   integer, parameter :: R8 = ESMF_KIND_R8

contains

   module function make_lataxis_from_metadata(file_metadata, rc) result(axis)
      type(LatAxis) :: axis
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      real(kind=R8), allocatable :: centers(:)
      real(kind=R8), allocatable :: corners(:)
      integer :: jm_world
      integer :: status
      character(:), allocatable :: dim_name

      dim_name = get_dim_name(file_metadata, units='degrees_north', _RC)
      centers = get_coordinates(file_metadata, dim_name, _RC)
      jm_world = size(centers)
      call fix_bad_pole(centers)
      corners = get_lat_corners(centers)
      ! fix corners
      if (corners(1) < -90) corners(1) = -90
      if (corners(jm_world+1) > 90) corners(jm_world+1) = 90

      axis = LatAxis(centers, corners)

      _RETURN(_SUCCESS)
   end function make_lataxis_from_metadata

end submodule make_lataxis_from_metadata_smod

