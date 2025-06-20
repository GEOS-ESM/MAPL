#include "MAPL_ErrLog.h"
submodule (mapl3g_CoordinateAxis) get_corners_smod
   implicit none(type,external)
   
contains
   
   pure module function get_corners(this) result(corners)
      real(kind=R8), allocatable :: corners(:)
      class(CoordinateAxis), intent(in) :: this

      corners = this%corners
      
   end function get_corners

end submodule get_corners_smod
