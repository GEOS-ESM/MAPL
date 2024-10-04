#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) xyz2latlon_smod
   use mapl_base, only: MAPL_GridGetCorners
contains

   pure module function xyz2latlon(xyz_coord) result(sph_coord)
      use MAPL_Constants, only: PI => MAPL_PI_R8
      real(kind=ESMF_KIND_R8), intent(in):: xyz_coord(3)
      real(kind=ESMF_KIND_R8) :: sph_coord(2)
      real(kind=ESMF_KIND_R8), parameter:: esl=1.e-10
      real(kind=ESMF_KIND_R8):: p(3)
      real(kind=ESMF_KIND_R8):: dist, lat, lon
      integer k

      p = xyz_coord
      dist =sqrt( dot_product(p,p))
      do k=1,3
         p(k) = p(k) / dist
      enddo

      if ( (abs(p(1))+abs(p(2)))  < esl ) then
         lon = 0.
      else
         lon = atan2( p(2), p(1) )   ! range [-pi,pi]
      endif

      if ( lon < 0.) lon = 2.*pi + lon
      lat = asin(p(3))

      sph_coord(1) = lon
      sph_coord(2) = lat

   end function xyz2latlon

end submodule xyz2latlon_smod
