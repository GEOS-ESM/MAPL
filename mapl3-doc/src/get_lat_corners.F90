#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) get_lat_corners_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none (type, external)

   integer, parameter :: R8 = ESMF_KIND_R8

contains

   module function get_lat_corners(centers) result(corners)
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), allocatable :: corners(:)

      associate (jm => size(centers))
        allocate(corners(jm+1))
         corners(1) = centers(1) - (centers(2)-centers(1))/2
         corners(2:jm) = (centers(1:jm-1) + centers(2:jm))/2
         corners(jm+1) = centers(jm) + (centers(jm)-centers(jm-1))/2
      end associate
   end function get_lat_corners

end submodule get_lat_corners_smod

