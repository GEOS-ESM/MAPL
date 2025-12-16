#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) get_idx_range_smod
   use mapl_ErrorHandlingMod
   implicit none

contains

   pure module subroutine get_idx_range(distribution, rank, i_0, i_1)
      integer, intent(in) :: distribution(:)
      integer, intent(in) :: rank
      integer, intent(out) :: i_0, i_1

      i_0 = 1 + sum(distribution(:rank))
      i_1 = i_0 + distribution(rank+1) - 1

   end subroutine get_idx_range

end submodule get_idx_range_smod

