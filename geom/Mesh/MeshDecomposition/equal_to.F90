#include "MAPL_ErrLog.h"

submodule (mapl3g_MeshDecomposition) equal_to_smod
   implicit none (type, external)

contains

   module procedure equal_to
      
      ! Both must have allocated distributions
      if (.not. allocated(decomp1%point_distribution)) then
         equal_to = .not. allocated(decomp2%point_distribution)
         return
      end if

      if (.not. allocated(decomp2%point_distribution)) then
         equal_to = .false.
         return
      end if

      ! Check if distributions are the same
      equal_to = all(decomp1%point_distribution == decomp2%point_distribution)

   end procedure equal_to

end submodule equal_to_smod
