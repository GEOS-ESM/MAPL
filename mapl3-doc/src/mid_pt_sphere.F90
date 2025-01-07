#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) mid_pt_sphere_smod
   use mapl_base, only: MAPL_GridGetCorners
contains


   ! Geometry utilities

   pure module function mid_pt_sphere(p1, p2) result(pm)
      real(kind=ESMF_KIND_R8) , intent(in)  :: p1(2), p2(2)
      real(kind=ESMF_KIND_R8) :: pm(2)
      real(kind=ESMF_KIND_R8) :: e1(3), e2(3), e3(3),dd

      e1 = latlon2xyz(p1)
      e2 = latlon2xyz(p2)
      e3 = e1 + e2
      dd = sqrt(dot_product(e3,e3))
      e3 = e3 / dd
      pm = xyz2latlon(e3)

   end function mid_pt_sphere

end submodule mid_pt_sphere_smod
