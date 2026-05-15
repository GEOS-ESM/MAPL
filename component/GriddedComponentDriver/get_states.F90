#include "MAPL.h"

submodule(mapl3g_GriddedComponentDriver) get_states_smod
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   module function get_states(this) result(states)
      type(MultiState) :: states
      class(GriddedComponentDriver), intent(in) :: this

      states = this%states
   end function get_states

end submodule get_states_smod
