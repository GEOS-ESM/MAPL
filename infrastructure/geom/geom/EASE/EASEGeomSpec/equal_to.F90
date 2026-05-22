#include "MAPL.h"

submodule (mapl_EASEGeomSpec_mod) equal_to_smod
   use mapl_GeomSpec_mod
   use mapl_EASEDecomposition_mod
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
