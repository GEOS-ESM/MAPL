#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) new_LatLonDecomposition_petcount_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none

contains

   pure module function new_LatLonDecomposition_petcount(dims, unusable, petCount) result(decomp)
      use mapl_KeywordEnforcer
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: petCount

      integer :: nx, nx_start

      associate (aspect_ratio => real(dims(1))/dims(2))
        nx_start = max(1, floor(sqrt(petCount * aspect_ratio)))
        do nx = nx_start, 1, -1
           if (mod(petcount, nx) == 0) then ! found a decomposition
              exit
           end if
        end do
      end associate

      decomp = LatLonDecomposition(dims, topology=[nx, petCount/nx])

   end function new_LatLonDecomposition_petcount

end submodule new_LatLonDecomposition_petcount_smod

