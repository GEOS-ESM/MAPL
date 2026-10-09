#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_accept_transfer_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_ACCEPT_TRANSFER
   use mapl_GeometrySpec_mod
   use mapl_MultiState_mod
   use mapl_Connection_mod
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_GraphBuilder_mod, only: GraphBuilder
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_GRAPH
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_accept_transfer(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(esmf_State) :: importState
      type(esmf_State) :: exportState
      type(esmf_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(MultiState) :: outer_states, user_states, tmp_states
      class(GriddedComponentDriver), pointer :: provider
      type(ESMF_GridComp) :: provider_gc
      type(OuterMetaComponent), pointer :: provider_meta
      integer :: status
      type(GraphBuilder) :: gb

!#      call this%propagate_geom_to_children(_RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_ACCEPT_TRANSFER, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, 'GENERIC::INIT_ACCEPT_TRANSFER', _RC)

      call process_connections(this, _RC)
      ! Phase 3b: process_connections() above calls Connection%connect()
      ! at this phase - the real wiring step (versus activate() at
      ! GENERIC_INIT_ADVERTISE, see initialize_advertise.F90). This is
      ! therefore the correct point for GraphBuilder's own real graph
      ! mutation (proxy nodes + dependency edges) and freeze, additional
      ! to, not a replacement for, process_connections() above - see
      ! mapl_GraphBuilder_mod module header, "Two-phase timing". Runs
      ! after recursion, so every child has already completed its own
      ! accept-transfer step (real edges + freeze) by this point, making
      ! cross-boundary proxy/lookup reads well-defined.
      call gb%run_connect_hook(this)
      call this%registry%propagate_exports(_RC)

      user_states = this%user_gc_driver%get_states()
      tmp_states = MultiState(importState=user_states%importState)
      ! openspec/changes/phase-10a-state-materialization, graph-mode
      ! spec "graph mode makes graph-native population the real
      ! producer": in GRAPH_MODE_GRAPH, graph-native population
      ! (immediately below) is the sole real producer for this
      ! component - legacy's own add_to_states is skipped entirely
      ! rather than running alongside it (never GRAPH_MODE_BOTH, which
      ! runs graph-native materialization but never touches a shared
      ! ESMF_State - design.md "both mode's no-collision guarantee is
      ! structural"). Not exercised end-to-end by any fixture in this
      ! phase (proposal.md/design.md's own explicit "demonstrate before
      ! replace" deferral) - verified only via graph_mode()'s own direct
      ! unit coverage and code-level inspection.
      if (graph_mode() /= GRAPH_MODE_GRAPH) then
         call this%registry%add_to_states(tmp_states, mode='user', _RC)
      end if
      call gb%run_populate_state_hook(this, tmp_states)

      outer_states = MultiState(importState=importState, exportState=exportState)
      if (graph_mode() /= GRAPH_MODE_GRAPH) then
         call this%registry%add_to_states(outer_states, mode='outer', _RC)
      end if

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(unusable)

   end subroutine initialize_accept_transfer

   subroutine process_connections(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c

      associate (e => this%component_spec%connections%end())
        iter = this%component_spec%connections%begin()
        do while (iter /= e)
           c => iter%of()
           call c%connect(this%registry, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine process_connections

end submodule initialize_accept_transfer_smod
