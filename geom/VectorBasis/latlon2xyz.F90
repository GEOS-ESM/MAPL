#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) latlon2xy_smod
contains

   pure module function latlon2xyz(sph_coord,right_hand) result(xyz_coord)
      real(kind=ESMF_KIND_R8), intent(in), dimension(2) :: sph_coord
      logical, intent(in), optional :: right_hand
      real(kind=ESMF_KIND_R8), dimension(3) :: xyz_coord

      logical :: rh_
      if (present(right_hand)) then
         rh_=right_hand
      else
         rh_=.true.
      end if
      xyz_coord(1) = cos(sph_coord(2)) * cos(sph_coord(1))
      xyz_coord(2) = cos(sph_coord(2)) * sin(sph_coord(1))
      if (rh_) then
         xyz_coord(3) = sin(sph_coord(2))
      else
         xyz_coord(3) = -sin(sph_coord(2))
      end if

   end function latlon2xyz

end submodule latlon2xy_smod
