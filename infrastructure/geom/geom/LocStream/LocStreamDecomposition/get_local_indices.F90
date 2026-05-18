#include "MAPL_ErrLog.h"

submodule (mapl3g_LocStreamDecomposition) get_local_indices_smod
   implicit none (type, external)

contains

   module procedure get_local_indices
      integer :: i

      i_0 = 1
      do i = 1, rank
         i_0 = i_0 + this%point_distribution(i)
      end do

      i_1 = i_0 + this%point_distribution(rank + 1) - 1

   end procedure get_local_indices

end submodule get_local_indices_smod
