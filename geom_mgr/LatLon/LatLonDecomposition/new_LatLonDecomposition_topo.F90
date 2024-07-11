#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) new_LatLonDecomposition_topo_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none

contains

   pure module function new_LatLonDecomposition_topo(dims, unusable, topology) result(decomp)
      use mapl_KeywordEnforcer
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: topology(2)

      allocate(decomp%lon_distribution(topology(1)))
      allocate(decomp%lat_distribution(topology(2)))

      call MAPL_DecomposeDim(dims(1), decomp%lon_distribution, topology(1), min_DE_extent=2)
      call MAPL_DecomposeDim(dims(2), decomp%lat_distribution, topology(2), min_DE_extent=2)

   end function new_LatLonDecomposition_topo

end submodule new_LatLonDecomposition_topo_smod

