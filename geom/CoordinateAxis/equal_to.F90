#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) equal_to_smod
   use mapl_ErrorHandling
   use gftl2_StringVector
   implicit none(type,external)

contains
   
   elemental logical module function equal_to(a, b)
      type(CoordinateAxis), intent(in) :: a, b

      ! Do the fast checks first
      equal_to = size(a%centers) == size(b%centers)
      if (.not. equal_to) return
      equal_to = size(a%corners) == size(b%corners)
      if (.not. equal_to) return

      equal_to = all(a%centers == b%centers)
      if (.not. equal_to) return
      equal_to = all(a%corners == b%corners)
   end function equal_to

end submodule equal_to_smod
