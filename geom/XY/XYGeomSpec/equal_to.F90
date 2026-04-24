#include "MAPL.h"

submodule (mapl3g_XYGeomSpec) equal_to_smod
   use mapl_ErrorHandlingMod
   implicit none (type, external)

contains

   pure logical module function equal_to(a, b)
      class(XYGeomSpec), intent(in) :: a
      class(GeomSpec),   intent(in) :: b

      select type (b)
      type is (XYGeomSpec)
         equal_to = (a%im_world   == b%im_world)   .and. &
                    (a%jm_world   == b%jm_world)    .and. &
                    (a%lm         == b%lm)           .and. &
                    (a%coord_mode == b%coord_mode)   .and. &
                    (a%thin_factor == b%thin_factor) .and. &
                    (a%grid_file_name == b%grid_file_name)
      class default
         equal_to = .false.
      end select

   end function equal_to

end submodule equal_to_smod
