#include "MAPL.h"

submodule (mapl_LatLonGeomSpec_mod) make_gridded_dims_smod

   use mapl_ErrorHandling_mod
   use gFTL2_StringVector

   implicit none (type, external)

contains

   module function make_gridded_dims(this, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(LatLonGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      gridded_dims = StringVector()
      call gridded_dims%push_back('lon')
      call gridded_dims%push_back('lat')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_gridded_dims


end submodule make_gridded_dims_smod
