#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) equal_to_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none

   integer, parameter :: R8 = ESMF_KIND_R8

contains

   elemental logical module function equal_to(a, b)
      type(LatAxis), intent(in) :: a, b
      equal_to = (a%CoordinateAxis == b%CoordinateAxis)
   end function equal_to

end submodule equal_to_smod

