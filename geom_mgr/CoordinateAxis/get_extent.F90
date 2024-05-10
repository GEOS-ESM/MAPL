#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) get_extent_smod
   use esmf, only: ESMF_UtilStringLowerCase
   use mapl_ErrorHandling
   use gftl2_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

contains
   
   ! Accessors
   !----------
   ! Note that size(this%corners) might be one larger for non-periodic
   pure module function get_extent(this) result(extent)
      class(CoordinateAxis), intent(in) :: this
      integer :: extent
      extent = size(this%centers)
   end function get_extent

end submodule get_extent_smod
