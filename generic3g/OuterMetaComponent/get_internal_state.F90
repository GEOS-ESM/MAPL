#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_internal_state_smod
   use mapl3g_Multistate
   implicit none

contains

   !TODO: put "user" in procedure name
   module function get_internal_state(this) result(internal_state)
      type(ESMF_State) :: internal_state
      class(OuterMetaComponent), intent(in) :: this

      type(MultiState) :: user_states

      user_states = this%user_gc_driver%get_states()
      internal_state = user_states%internalState

   end function get_internal_state

end submodule get_internal_state_smod
