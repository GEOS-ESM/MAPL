#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) not_equal_to_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none

contains

   elemental module function not_equal_to(decomp1, decomp2)
      logical :: not_equal_to
      type(LatLonDecomposition), intent(in) :: decomp1
      type(LatLonDecomposition), intent(in) :: decomp2

      not_equal_to = .not. (decomp1 == decomp2)

   end function not_equal_to

end submodule not_equal_to_smod

