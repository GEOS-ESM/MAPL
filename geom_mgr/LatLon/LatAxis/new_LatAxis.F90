#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) new_LatAxis_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none

   integer, parameter :: R8 = ESMF_KIND_R8

contains

   ! Constructor
   pure module function new_LatAxis(centers, corners) result(axis)
      type(LatAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)
      axis%CoordinateAxis = CoordinateAxis(centers, corners)
   end function new_LatAxis

end submodule new_LatAxis_smod

