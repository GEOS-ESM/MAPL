#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_component_spec_smod
   implicit none

contains

   module function get_component_spec(this) result(component_spec)
      type(ComponentSpec), pointer :: component_spec
      class(OuterMetaComponent), target, intent(in) :: this
      component_spec => this%component_spec
   end function get_component_spec

end submodule get_component_spec_smod
