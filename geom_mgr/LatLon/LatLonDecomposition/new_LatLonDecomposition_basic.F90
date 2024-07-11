#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) new_LatLonDecomposition_basic_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none

contains

   pure module function new_LatLonDecomposition_basic(lon_distribution, lat_distribution) result(decomp)
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: lon_distribution(:)
      integer, intent(in) :: lat_distribution(:)

      decomp%lon_distribution = lon_distribution
      decomp%lat_distribution = lat_distribution

   end function new_LatLonDecomposition_basic

end submodule new_LatLonDecomposition_basic_smod

