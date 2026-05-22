#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_hconfig_smod
   implicit none(type,external)

contains

   module function get_hconfig(this) result(hconfig)
      type(ESMF_Hconfig) :: hconfig
      class(OuterMetaComponent), intent(inout) :: this

      hconfig = this%hconfig

   end function get_hconfig

end submodule get_hconfig_smod
