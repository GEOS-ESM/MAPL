#include "MAPL.h"
submodule (mapl_CoordinateAxis_mod) get_tolerance_smod
   implicit none(type,external)

contains

   pure module function get_tolerance(this) result(tolerance)
      real(kind=R8) :: tolerance
      class(CoordinateAxis), intent(in) :: this

      tolerance = this%tolerance

   end function get_tolerance

end submodule get_tolerance_smod
