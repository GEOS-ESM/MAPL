#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) get_corners_smod
   use esmf, only: ESMF_UtilStringLowerCase
   use mapl_ErrorHandling
   use gftl2_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

contains
   
   pure module function get_corners(this) result(corners)
      real(kind=R8), allocatable :: corners(:)
      class(CoordinateAxis), intent(in) :: this

      corners = this%corners
      
   end function get_corners

end submodule get_corners_smod
