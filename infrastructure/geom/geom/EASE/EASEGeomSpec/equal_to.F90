#include "MAPL.h"

submodule (mapl3g_EASEGeomSpec) equal_to_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEDecomposition
   implicit none (type, external)

contains

   pure logical module function equal_to(a, b)
      class(EASEGeomSpec), intent(in) :: a
      class(GeomSpec),     intent(in) :: b

      select type (b)
      type is (EASEGeomSpec)
         equal_to = (a%grid_name == b%grid_name) .and. &
                    (a%decomposition == b%decomposition)
      class default
         equal_to = .false.
      end select

   end function equal_to

end submodule equal_to_smod
