#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_phases_smod
   implicit none

contains

   module function get_phases(this, method_flag) result(phases)
      type(StringVector), pointer :: phases
      class(OuterMetaComponent), target, intent(inout):: this
      type(ESMF_Method_Flag), intent(in) :: method_flag

      phases => this%user_phases_map%of(method_flag)

   end function get_phases

end submodule get_phases_smod
