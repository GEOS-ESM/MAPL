#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) not_equal_to_smod
   use esmf, only: ESMF_UtilStringLowerCase
   use mapl_ErrorHandling
   use gftl2_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

contains
   

   elemental logical module function not_equal_to(a, b)
      type(CoordinateAxis), intent(in) :: a, b

      not_equal_to = .not. (a == b)
   end function not_equal_to

end submodule not_equal_to_smod
