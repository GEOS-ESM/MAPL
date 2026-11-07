! Whether the graph, not legacy, populates the real user import, internal
! and export ESMF_States. Off by default: legacy fills them and the graph
! only creates, completes and aliases Fields. On (GRAPH mode): legacy
! skips its user-state population and graph population fills all three.
! Outer states stay legacy's either way.
module mapl_GraphOwnsUserStates_mod
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_GRAPH
   implicit none
   private

   public :: set_graph_owns_user_states
   public :: graph_owns_user_states
   public :: graph_populates_user_states

   logical, save :: owns_user_states = .false.

contains

   ! Test-only setter; no production code calls it.
   subroutine set_graph_owns_user_states(enabled)
      logical, intent(in) :: enabled

      owns_user_states = enabled
   end subroutine set_graph_owns_user_states

   logical function graph_owns_user_states() result(enabled)
      enabled = owns_user_states
   end function graph_owns_user_states

   ! The switch only takes effect in GRAPH mode; every gate that skips legacy
   ! or fills the user states asks this, never the bare switch.
   logical function graph_populates_user_states() result(populates)
      populates = owns_user_states .and. graph_mode() == GRAPH_MODE_GRAPH
   end function graph_populates_user_states

end module mapl_GraphOwnsUserStates_mod
