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
   use mapl_GraphOwnsUserStates_mod, only: graph_populates_user_states
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use mapl_GraphResolutionEntryVector_mod, only: GraphResolutionEntryVector
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

      type(MultiState) :: outer_states, user_states, tmp_states, graph_states
      class(GriddedComponentDriver), pointer :: provider
      type(ESMF_GridComp) :: provider_gc
      type(OuterMetaComponent), pointer :: provider_meta
      integer :: status
      type(GraphBuilder) :: gb
      type(GraphResolutionEntryVector) :: expression_unresolved
      type(GraphResolutionEntryVector) :: expression_unsupported

!#      call this%propagate_geom_to_children(_RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_ACCEPT_TRANSFER, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, 'GENERIC::INIT_ACCEPT_TRANSFER', _RC)

      call process_connections(this, _RC)
      ! process_connections() above calls Connection%connect() at this
      ! phase - the real wiring step (versus activate() at
      ! GENERIC_INIT_ADVERTISE, see initialize_advertise.F90). So this is
      ! where GraphBuilder adds its dependency edges, in addition to
      ! process_connections() (see mapl_GraphBuilder_mod header,
      ! "Two-phase timing"). Runs after recursion, so every child has
      ! already completed its own accept-transfer step by this point and
      ! cross-boundary lookups are well-defined.
      call gb%run_connect_hook(this)
      ! After run_connect_hook (sibling exports are advertised) and before
      ! propagate_exports. A problem in an expression is fatal in every
      ! validation mode.
      call gb%run_expression_hook(this, unresolved=expression_unresolved, &
           unsupported=expression_unsupported, _RC)
      call fail_on_expression_reports(expression_unresolved, &
           expression_unsupported, _RC)
      call this%registry%propagate_exports(_RC)

      user_states = this%user_gc_driver%get_states()
      tmp_states = MultiState(importState=user_states%importState)
      ! Legacy add_to_states is skipped in GRAPH mode, so legacy and the
      ! graph do not both fill the user states. When
      ! graph_populates_user_states() is on, the graph fills the real user
      ! states here and the outer states at realize_accepted
      ! (initialize_realize_accepted.F90).
      if (graph_mode() /= GRAPH_MODE_GRAPH) then
         call this%registry%add_to_states(tmp_states, mode='user', _RC)
      end if
      ! Off: legacy filled the user states and the graph populates only
      ! what it always did (the real import state; export and internal go to
      ! throwaway states). On: the graph fills all three real user states.
      if (graph_populates_user_states()) then
         graph_states = user_states
      else
         graph_states = tmp_states
      end if
      call gb%run_populate_state_hook(this, graph_states)

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

   ! Fails with every problem the expression hook reported, one message
   ! per problem.
   subroutine fail_on_expression_reports(unresolved, unsupported, rc)
      type(GraphResolutionEntryVector), target, intent(in) :: unresolved
      type(GraphResolutionEntryVector), target, intent(in) :: unsupported
      integer, optional, intent(out) :: rc

      character(:), allocatable :: message
      type(GraphResolutionEntry), pointer :: entry
      integer :: i

      message = ''
      do i = 1, unresolved%size()
         entry => unresolved%of(i)
         call append('GraphBuilder: ' // entry%reason)
      end do
      do i = 1, unsupported%size()
         entry => unsupported%of(i)
         call append('GraphBuilder: expression ' // entry%short_name // &
              ': ' // entry%reason)
      end do
      if (message /= '') then
         _FAIL(message)
      end if

      _RETURN(_SUCCESS)
   contains
      subroutine append(text)
         character(*), intent(in) :: text
         if (message /= '') message = message // new_line('a')
         message = message // text
      end subroutine append
   end subroutine fail_on_expression_reports

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
