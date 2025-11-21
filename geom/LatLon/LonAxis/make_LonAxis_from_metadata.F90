#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) make_LonAxis_from_metadata_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   module function make_LonAxis_from_metadata(file_metadata, rc) result(axis)
      type(LonAxis) :: axis
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      real(kind=R8), allocatable :: centers(:)
      real(kind=R8), allocatable :: corners(:)
      integer :: im_world
      integer :: status
      character(:), allocatable :: dim_name

      dim_name = get_dim_name(file_metadata, units='degrees_east', _RC)
      centers = get_coordinates(file_metadata, dim_name, _RC)
      im_world = size(centers)
      ! Enforce convention for longitude range.
      if (any((centers(2:im_world) - centers(1:im_world-1)) < 0)) then
         where(centers > 180) centers = centers - 360
      end if
      corners = get_lon_corners(centers)
      axis = LonAxis(centers, corners)

      _RETURN(_SUCCESS)
   end function make_LonAxis_from_metadata

end submodule make_LonAxis_from_metadata_smod

