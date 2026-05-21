#include "MAPL.h"

submodule (mapl_OuterMetaComponent) get_registry_smod
   implicit none(type,external)

contains

   module function get_registry(this) result(registry)
      type(StateRegistry), pointer :: registry
      class(OuterMetaComponent), target, intent(in) :: this

      registry => this%registry
   end function get_registry


end submodule get_registry_smod
