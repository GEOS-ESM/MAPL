#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) equal_to_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   
contains

   pure logical module function equal_to(a, b)
      class(LatLonGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      select type (b)
      type is (LatLonGeomSpec)
         equal_to = (a%lon_axis == b%lon_axis) .and. (a%lat_axis == b%lat_axis)
         if (.not. equal_to) return
         equal_to = (a%decomposition == b%decomposition)
      class default
         equal_to = .false.
      end select

   end function equal_to

end submodule equal_to_smod
