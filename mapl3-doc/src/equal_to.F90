#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) equal_to_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none (type, external)

contains

   elemental module function equal_to(decomp1, decomp2)
      logical :: equal_to
      type(LatLonDecomposition), intent(in) :: decomp1
      type(LatLonDecomposition), intent(in) :: decomp2

      equal_to = size(decomp1%lon_distribution) == size(decomp2%lon_distribution)
      if (.not. equal_to) return

      equal_to = size(decomp1%lat_distribution) == size(decomp2%lat_distribution)
      if (.not. equal_to) return

      equal_to = all(decomp1%lon_distribution == decomp2%lon_distribution)
      if (.not. equal_to) return

      equal_to = all(decomp1%lat_distribution == decomp2%lat_distribution)

   end function equal_to

end submodule equal_to_smod

