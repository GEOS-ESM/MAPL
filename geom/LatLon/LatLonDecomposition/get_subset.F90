#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) get_subset_smod
   use mapl_ErrorHandlingMod
   implicit none

contains

   pure module function get_subset(coordinates, i_0, i_1) result(subset)
      real(kind=R8), allocatable :: subset(:)
      real(kind=R8), intent(in) :: coordinates(:)
      integer, intent(in) :: i_0, i_1

      subset = coordinates(i_0:i_1)

   end function get_subset

end submodule get_subset_smod

