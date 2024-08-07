#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) equal_to_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
   use esmf
   implicit none
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   elemental logical module function equal_to(a, b)
      type(LonAxis), intent(in) :: a, b
      equal_to = (a%CoordinateAxis == b%CoordinateAxis)
   end function equal_to


end submodule equal_to_smod

