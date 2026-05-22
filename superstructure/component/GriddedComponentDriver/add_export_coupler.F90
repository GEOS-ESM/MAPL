#include "MAPL.h"

submodule (mapl_GriddedComponentDriver_mod) add_export_coupler_smod
   implicit none

contains

   module subroutine add_export_coupler(this, driver)
      class(GriddedComponentDriver), intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: driver
      call this%export_couplers%push_back(driver)
   end subroutine add_export_coupler

end submodule add_export_coupler_smod
