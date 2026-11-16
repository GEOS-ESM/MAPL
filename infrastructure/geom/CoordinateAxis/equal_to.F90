#include "MAPL.h"

submodule (mapl_CoordinateAxis_mod) equal_to_smod
   use mapl_ErrorHandling_mod
   use gftl2_StringVector
   implicit none(type,external)

contains
   
   elemental logical module function equal_to(a, b)
      type(CoordinateAxis), intent(in) :: a, b

      real(kind=R8) :: abs_tol

      ! Do the fast checks first
      equal_to = size(a%centers) == size(b%centers)
      if (.not. equal_to) return
      equal_to = size(a%corners) == size(b%corners)
      if (.not. equal_to) return

      ! Directional, not symmetric: `a` is the already-registered axis;
      ! `b` is the new candidate not yet in the registry (see gFTL's
      ! find(), which calls T_EQ(container_element, lookup_value), i.e.
      ! equal_to(a=container_element, b=lookup_value)). Only `b` gets to
      ! decide, using its own tolerance and its own local grid spacing
      ! (DX), whether `a` is "close enough". `a`'s tolerance is never
      ! consulted here.
      abs_tol = b%tolerance * min_spacing(b%centers)

      equal_to = all(abs(a%centers - b%centers) <= abs_tol)
      if (.not. equal_to) return
      equal_to = all(abs(a%corners - b%corners) <= abs_tol)
   end function equal_to

   ! Minimum spacing between adjacent centers, i.e. the local DX used to
   ! scale a fractional coordinate_tolerance into an absolute value. A
   ! degenerate single-point axis has no defined spacing and yields 0
   ! (so its tolerance has no effect - strict/bitwise comparison).
   pure function min_spacing(centers) result(dx)
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8) :: dx

      if (size(centers) < 2) then
         dx = 0.0_R8
      else
         dx = minval(abs(centers(2:) - centers(:size(centers)-1)))
      end if
   end function min_spacing

end submodule equal_to_smod
