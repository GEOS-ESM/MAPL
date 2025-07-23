#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) set_hconfig_smod
   implicit none

contains

   module subroutine set_hconfig(this, hconfig)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_HConfig), intent(in) :: hconfig

      this%hconfig = hconfig

   end subroutine set_hconfig

end submodule set_hconfig_smod
