#include "MAPL.h"

submodule (mapl_CoordinateAxis_mod) new_CoordinateAxis_smod
   use mapl_ErrorHandling_mod
   use gftl2_StringVector
   implicit none(type,external)

contains
   
   pure module function new_CoordinateAxis(centers, corners, tolerance) result(axis)
      type(CoordinateAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)
      real(kind=R8), optional, intent(in) :: tolerance

      axis%centers = centers
      axis%corners = corners
      if (present(tolerance)) axis%tolerance = tolerance
   end function new_CoordinateAxis


end submodule new_CoordinateAxis_smod
