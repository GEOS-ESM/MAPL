#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) get_unit_vector_smod
contains


   ! Utility functions
   !------------------
   pure module function get_unit_vector( p1, p2, p3 ) result(uvect)
      real(kind=ESMF_KIND_R8), intent(in):: p1(2), p2(2), p3(2) 
      real(kind=ESMF_KIND_R8) :: uvect(3) 
      real(kind=ESMF_KIND_R8) :: xyz1(3), xyz2(3), xyz3(3)
      real(kind=ESMF_KIND_R8) :: ap 

      xyz1 = latlon2xyz(p1,right_hand=.true.)
      xyz2 = latlon2xyz(p2,right_hand=.true.)
      xyz3 = latlon2xyz(p3,right_hand=.true.)
      uvect = xyz3-xyz1

      ap = dot_product(uvect,xyz2) 
      uvect = uvect - ap*xyz2
      ap = dot_product(uvect,uvect)
      uvect=uvect/sqrt(ap)

   end function get_unit_vector


end submodule get_unit_vector_smod
