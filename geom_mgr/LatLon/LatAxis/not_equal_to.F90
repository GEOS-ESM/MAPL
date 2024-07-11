#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) not_equal_to_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none

   integer, parameter :: R8 = ESMF_KIND_R8

contains

   elemental logical module function not_equal_to(a, b)
      type(LatAxis), intent(in) :: a, b
      not_equal_to = .not. (a == b)
   end function not_equal_to

end submodule not_equal_to_smod

