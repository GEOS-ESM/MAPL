#include "MAPL_ErrLog.h"

submodule (mapl3g_LatAxis) fix_bad_pole_smod
   use mapl_RangeMod
!   use hconfig3g
   use esmf
   use mapl_ErrorHandling
   implicit none (type, external)

   integer, parameter :: R8 = ESMF_KIND_R8

contains

   ! Magic code from ancient times.
   ! Do not touch unless you understand ...
   module subroutine fix_bad_pole(centers)
      real(kind=R8), intent(inout) :: centers(:)

      integer :: n
      real(kind=R8) :: d_lat, extrap_lat
      real, parameter :: tol = 1.0e-5

      if (size(centers) < 4) return ! insufficient data

      ! Check: is this a "mis-specified" pole-centered grid?
      ! Assume lbound=1 and ubound=size for now

      n = size(centers)
      d_lat = (centers(n-1) - centers(2)) / (n - 3)

      ! Check: is this a regular grid (i.e. constant spacing away from the poles)?
      if (any(((centers(2:n-1) - centers(1:n-2)) - d_lat) < tol*d_lat)) return

      ! Should the southernmost point actually be at the pole?
      extrap_lat = centers(2) - d_lat
      if (extrap_lat <= ((d_lat/20.0)-90.0)) then
         centers(1) = -90.0
      end if

      ! Should the northernmost point actually be at the pole?
      extrap_lat = centers(n-1) + d_lat
      if (extrap_lat >= (90.0-(d_lat/20.0))) then
         centers(n) =  90.0
      end if

   end subroutine fix_bad_pole

end submodule fix_bad_pole_smod

