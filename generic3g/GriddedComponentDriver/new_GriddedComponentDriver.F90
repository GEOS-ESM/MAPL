#include "MAPL_Generic.h"

submodule (mapl3g_GriddedComponentDriver) new_GriddedComponentDriver_smod
   implicit none (type, external)

contains

   module function new_GriddedComponentDriver(gridcomp, clock, states) result(child)
      type(GriddedComponentDriver) :: child
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(ESMF_Clock), intent(in) :: clock
      type(MultiState), intent(in) :: states

      child%gridcomp = gridcomp
      child%clock = clock
      child%states = states

   end function new_GriddedComponentDriver

end submodule new_GriddedComponentDriver_smod
