#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_hconfig_smod
   implicit none

contains

   module function get_hconfig(this) result(hconfig)
      type(ESMF_Hconfig) :: hconfig
      class(OuterMetaComponent), intent(inout) :: this

      hconfig = this%hconfig

   end function get_hconfig

end submodule get_hconfig_smod
