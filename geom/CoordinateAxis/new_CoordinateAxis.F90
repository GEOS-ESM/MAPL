#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) new_CoordinateAxis_smod
   use esmf, only: ESMF_UtilStringLowerCase
   use mapl_ErrorHandling
   use gftl2_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

contains
   
   pure module function new_CoordinateAxis(centers, corners) result(axis)
      type(CoordinateAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)

      axis%centers = centers
      axis%corners = corners
   end function new_CoordinateAxis


end submodule new_CoordinateAxis_smod
