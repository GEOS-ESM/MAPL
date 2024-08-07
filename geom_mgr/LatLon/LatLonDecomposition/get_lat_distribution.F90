#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) get_lat_distribution_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none

contains

   pure module function get_lat_distribution(decomp) result(lat_distribution)
      integer, allocatable :: lat_distribution(:)
      class(LatLonDecomposition), intent(in) :: decomp
      lat_distribution = decomp%lat_distribution
   end function get_lat_distribution

end submodule get_lat_distribution_smod

