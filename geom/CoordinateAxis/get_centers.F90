#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) get_centers_smod
   use mapl_ErrorHandling
   use gftl2_StringVector
   implicit none(type,external)

contains
   
   pure module function get_centers(this) result(centers)
      real(kind=R8), allocatable :: centers(:)
      class(CoordinateAxis), intent(in) :: this

      centers = this%centers
      
   end function get_centers
      
end submodule get_centers_smod
