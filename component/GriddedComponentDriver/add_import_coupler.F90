#include "MAPL.h"

submodule (mapl3g_GriddedComponentDriver) add_import_coupler_smod
   implicit none

contains

   module subroutine add_import_coupler(this, driver)
      class(GriddedComponentDriver), intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: driver
      call this%import_couplers%push_back(driver)
   end subroutine add_import_coupler


end submodule add_import_coupler_smod
