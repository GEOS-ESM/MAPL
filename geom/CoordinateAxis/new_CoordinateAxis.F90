#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) new_CoordinateAxis_smod
   use mapl_ErrorHandling
   use gftl2_StringVector
   implicit none(type,external)

contains
   
   pure module function new_CoordinateAxis(centers, corners) result(axis)
      type(CoordinateAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)

      axis%centers = centers
      axis%corners = corners
   end function new_CoordinateAxis


end submodule new_CoordinateAxis_smod
