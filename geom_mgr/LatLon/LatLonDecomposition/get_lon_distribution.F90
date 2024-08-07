#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) get_lon_distribution_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none

contains

   ! accessors
   pure module function get_lon_distribution(decomp) result(lon_distribution)
      integer, allocatable :: lon_distribution(:)
      class(LatLonDecomposition), intent(in) :: decomp
      lon_distribution = decomp%lon_distribution
   end function get_lon_distribution
   
end submodule get_lon_distribution_smod

