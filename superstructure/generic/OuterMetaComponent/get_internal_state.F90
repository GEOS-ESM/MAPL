#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_internal_state_smod
   implicit none(type,external)

contains

   !TODO: put "user" in procedure name
   module function get_internal_state(this) result(internal_state)
      type(ESMF_State) :: internal_state
      class(OuterMetaComponent), intent(in) :: this

      type(MultiState) :: user_states

      ! Returns the thread local internal state while threading is active.
      user_states = this%get_thread_states()
      internal_state = user_states%internalState

   end function get_internal_state

end submodule get_internal_state_smod
