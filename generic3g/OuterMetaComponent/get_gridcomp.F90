#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_gridcomp_smod
   implicit none

contains

   ! Needed for unit testing purposes.

   module function get_gridcomp(this) result(gridcomp)
      type(ESMF_GridComp) :: gridcomp
      class(OuterMetaComponent), intent(in) :: this
      gridcomp = this%self_gridcomp
   end function get_gridcomp

end submodule get_gridcomp_smod
