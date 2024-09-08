#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) supports_metadata_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   logical module function supports_metadata(file_metadata, rc) result(supports)
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: dim_name

      supports = .true.
      dim_name = get_dim_name(file_metadata, units='degrees_east', _RC)

      supports = (dim_name /= '')
      _RETURN(_SUCCESS)
   end function supports_metadata

end submodule supports_metadata_smod

