#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) is_periodic_smod
   use mapl_ErrorHandling
   use gftl2_StringVector
   implicit none(type,external)

contains
   
   pure logical module function is_periodic(this)
      class(CoordinateAxis), intent(in) :: this

      real(kind=R8) :: span, spacing
      real(kind=R8), parameter :: tolerance = 0.01
      
      associate (corners => this%corners)
        associate (n => size(corners))

          if (n == 1) then
             is_periodic = .false.
             return
          end if
        
          span = corners(n) - corners(1)
          spacing = corners(2) - corners(1)

          if (abs(span - 360) < (tolerance * spacing)) then
             is_periodic = .true.
          else
             is_periodic = .false.
          end if

        end associate
      end associate
      
   end function is_periodic

end submodule is_periodic_smod
