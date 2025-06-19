#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) not_equal_to_smod
   use mapl_ErrorHandling
   use gftl2_StringVector
   implicit none(type,external)

contains
   

   elemental logical module function not_equal_to(a, b)
      type(CoordinateAxis), intent(in) :: a, b

      not_equal_to = .not. (a == b)
   end function not_equal_to

end submodule not_equal_to_smod
