#include "MAPL_ErrLog.h"

submodule (mapl3g_LonAxis) get_lon_corners_smod
   use mapl_RangeMod
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   integer, parameter :: R8 = ESMF_KIND_R8

contains

   module function get_lon_corners(centers) result(corners)
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), allocatable :: corners(:)

      associate (im => size(centers))
        allocate(corners(im+1))
        corners(1) = (centers(im) + centers(1))/2 - 180
        corners(2:im) = (centers(1:im-1) + centers(2:im))/2
        corners(im+1) = (centers(im) + centers(1))/2 + 180
      end associate
   end function get_lon_corners

end submodule get_lon_corners_smod

