#include "MAPL.h"

submodule (mapl3g_EASEGeomSpec) make_EASEGeomSpec_from_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEConversion
   use mapl3g_EASEDecomposition
   use pfio
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)

contains

   module function make_EASEGeomSpec_from_metadata(file_metadata, rc) result(spec)
      type(EASEGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status, im, cols, rows
      character(:), allocatable :: grid_name
      type(EASEDecomposition) :: decomposition

      if (file_metadata%has_dimension('lon')) then
         im = file_metadata%get_dimension('lon', _RC)
      else if (file_metadata%has_dimension('longitude')) then
         im = file_metadata%get_dimension('longitude', _RC)
      else
         _FAIL('make_EASEGeomSpec_from_metadata: no lon/longitude dimension found')
      end if

      grid_name = get_ease_gridname_by_cols(im, _RC)
      call ease_extent(grid_name, cols, rows, _RC)
      decomposition = make_EASEDecomposition([cols, rows], _RC)
      spec = EASEGeomSpec(grid_name, decomposition, _RC)

      _RETURN(_SUCCESS)
   end function make_EASEGeomSpec_from_metadata

end submodule make_EASEGeomSpec_from_metadata_smod
