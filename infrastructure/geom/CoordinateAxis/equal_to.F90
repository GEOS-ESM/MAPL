#include "MAPL.h"

submodule (mapl_CoordinateAxis_mod) equal_to_smod
   use mapl_ErrorHandling_mod
   use gftl2_StringVector
   implicit none(type,external)

contains
   
   elemental logical module function equal_to(a, b)
      type(CoordinateAxis), intent(in) :: a, b

      real(kind=R8) :: tol

      ! Do the fast checks first
      equal_to = size(a%centers) == size(b%centers)
      if (.not. equal_to) return
      equal_to = size(a%corners) == size(b%corners)
      if (.not. equal_to) return

      ! Symmetric combination rule: if either side opts into a nonzero
      ! tolerance, the comparison is tolerant. Default (both 0) preserves
      ! exact/bitwise comparison.
      tol = max(a%tolerance, b%tolerance)

      equal_to = all(abs(a%centers - b%centers) <= tol)
      if (.not. equal_to) return
      equal_to = all(abs(a%corners - b%corners) <= tol)
   end function equal_to

end submodule equal_to_smod
