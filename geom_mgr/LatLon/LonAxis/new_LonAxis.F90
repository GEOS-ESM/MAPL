#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) new_LonAxis_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
   use esmf
   implicit none
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   ! Constructor
   pure module function new_LonAxis(centers, corners) result(axis)
      type(LonAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)
      axis%CoordinateAxis = CoordinateAxis(centers, corners)
   end function new_LonAxis

end submodule new_LonAxis_smod

