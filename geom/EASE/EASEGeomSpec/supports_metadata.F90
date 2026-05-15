#include "MAPL.h"

submodule (mapl3g_EASEGeomSpec) supports_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEConversion
   use pfio
   use mapl_ErrorHandling
   implicit none (type, external)

contains

   ! A FileMetadata object is an EASE grid if its 'lon' (or 'longitude')
   ! dimension count matches one of the known EASE grid column counts.
   logical module function supports_metadata_(this, file_metadata, rc) result(supports)
      class(EASEGeomSpec), intent(in) :: this
      type(FileMetadata),  intent(in) :: file_metadata
      integer, optional,   intent(out) :: rc

      integer :: status, im
      character(:), allocatable :: grid_name

      supports = .false.

      ! Try 'lon' first, then 'longitude'
      if (file_metadata%has_dimension('lon')) then
         im = file_metadata%get_dimension('lon', _RC)
      else if (file_metadata%has_dimension('longitude')) then
         im = file_metadata%get_dimension('longitude', _RC)
      else
         _RETURN(_SUCCESS)
      end if

      ! If get_ease_gridname_by_cols succeeds, this is a recognized EASE grid
      grid_name = get_ease_gridname_by_cols(im, rc=status)
      supports = (status == _SUCCESS)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_metadata_

end submodule supports_metadata_smod
