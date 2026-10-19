#include "MAPL.h"

! openspec/changes/phase-10a-state-materialization: graph-native state population
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_StatePopulation_smod
   use esmf, only: ESMF_State, ESMF_StateAddReplace, ESMF_STATEITEM_FIELD, operator(==)
   use esmf, only: ESMF_Info, ESMF_NamedAliasGet, ESMF_InfoGetFromHost
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_FieldInfo_mod, only: FieldInfoSetInternal
   use mapl_RestartModes_mod, only: RestartMode
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_GRAPH
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Design correction (found while writing this sub-change's own pFUnit
   ! coverage, before any task was marked complete): the original
   ! decision - walk `this`'s own `ComponentSpec%var_specs` via
   ! `item_key`, mirroring `advertise_one` - only ever finds a
   ! materialized node for a connection declared and resolved entirely
   ! within one component, which no real fixture uses. For the ordinary
   ! cross-component case (a parent declares a `MatchConnection` between
   ! two of its own children - the only shape
   ! `Test_GraphBuilderEquivalence.pf`'s own fixture family uses), the
   ! resolved item is each child's own real `StateItemNode`, in that
   ! child's own graph (openspec/changes/hierarchy-wide-component-graph:
   ! no parent-local proxy exists anymore, tasks.md 3.1/3.2) - never a
   ! node in the PARENT's own graph. Walking `this`'s own `var_specs` +
   ! `item_key` alone would therefore find nothing whenever `this` is
   ! one of the two connected children (which is exactly when
   ! `initialize_accept_transfer.F90` calls this on each of them, once
   ! per component in the recursive tree).
   !
   ! Fixed by also walking each of `this`'s own declared children's
   ! VariableSpecs (`get_num_children`/`get_child_name`/
   ! `get_child_component_spec` - same enumeration
   ! `graphbuilder_resolve_geometry`'s own push-direction loop already
   ! uses), looking each one up via `item_key` in THAT CHILD's OWN
   ! graph (`get_child_component_graph`) - the child's own real NodeId,
   ! never a parent-local stand-in. The dependency EDGE itself still
   ! lives in `this`'s own default network (`net`, below) - that part
   ! is unaffected by this change, since `this` is still the component
   ! that declared the connection (design.md Non-Goals: "which network
   ! a declared connection populates stays exactly as declaration-
   ! scoped as before"). Node lookup (`graph%get_node()`) works
   ! correctly from ANY ComponentGraph instance within the same
   ! hierarchy regardless of which one is passed, since node storage is
   ! shared hierarchy-wide - so `populate_one` below is called exactly
   ! as before (passing `graph`/`net`, both this's own), only `id_ptr`
   ! now comes from the child's own graph instead of a `this`-local
   ! proxy lookup. An item with no resolved real NodeId (never
   ! connected) is silently skipped, same as an unresolved import. The
   ! same-component (`item_key`-based) walk is kept as the degenerate
   ! case for an item `this` advertises and connects entirely to
   ! itself, if that ever occurs.
   !
   ! Only a real producer when the graph mode is GRAPH_MODE_GRAPH
   ! (design.md "Graph-native state population is a new procedure"). A
   ! no-op (inserts nothing) in GRAPH_MODE_REGISTRY or GRAPH_MODE_BOTH -
   ! design.md's "both mode's no-collision guarantee is structural"
   ! decision: this is the ONLY gate on the real ESMF_StateAdd call
   ! below, so `both` mode can never reach it.
   !
   ! An export's own field is whatever is on its own node (materialized
   ! by resolve_one's no-mismatch branch above, or by the mismatch
   ! branch's existing try_materialize_field_extension). An import's
   ! real field is resolved by walking the default network's own
   ! predecessor edge back to the first node with a real field payload
   ! (find_materialized_predecessor below) - collapsing correctly for
   ! both the direct exact-match case (single hop) and an extension
   ! chain (walks through one or more transform nodes to the chain's own
   ! final node), with no special-casing between the two. INTERNAL items
   ! and non-Field items are skipped (same Field-only scope
   ! materialization itself already has); an import with no resolved
   ! predecessor (unresolved) is skipped silently, not an error (spec
   ! "Unresolved import is not populated").
   module subroutine graphbuilder_populate_state(this, multi_state, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: this_spec, child_spec
      type(ComponentGraph), pointer :: graph
      type(ComponentGraph), pointer :: child_graph
      type(DependencyNetwork), pointer :: net
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(NodeId), pointer :: id_ptr
      character(:), allocatable :: child_name
      integer :: n, i
      type(OuterMetaComponent), pointer :: child_meta
      type(GriddedComponentDriver), pointer :: child_driver
      type(MultiState) :: child_states

      _RETURN_IF(graph_mode() /= GRAPH_MODE_GRAPH)

      graph => this%get_component_graph()
      net => graph%get_network(graph%get_default_network_id())

      ! Degenerate same-component case: an item `this` advertises and
      ! connects entirely to itself - item_key, this's own native node.
      this_spec => this%get_component_spec()
      associate (e => this_spec%var_specs%ftn_end())
         iter = this_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT .or. &
                 var_spec%state_intent == ESMF_STATEINTENT_EXPORT)) cycle
            id_ptr => graph%get_resource_index(item_key(var_spec%state_intent, var_spec%short_name))
            call populate_one(graph, net, multi_state, id_ptr, var_spec%state_intent, var_spec%short_name, _RC)
         end do
      end associate

      ! Ordinary cross-component case: each declared child's own
      ! advertised import/export item is resolved against THAT CHILD'S
      ! OWN graph (item_key) - its own real NodeId, no parent-local
      ! proxy. The dependency edge connecting it to its counterpart
      ! still lives in `this`'s own default network (`net`, above),
      ! since `this` is the component that declared the connection.
      !
      ! openspec/changes/graph-child-state-population, design.md D1:
      ! unlike the degenerate same-component branch above (which keeps
      ! using the procedure's own `multi_state` argument - that one IS
      ! this component's own real state), the resolved item here
      ! belongs to a CHILD this component merely connected, never to
      ! `this` itself - `this` (the parent) has no stake in the real
      ! ESMF_State the item is populated into. Each iteration therefore
      ! looks up that child's own OuterMetaComponent (the one-way
      ! parent-to-child reach `get_child_component_graph`/
      ! `get_child_component_spec` already establish elsewhere in this
      ! module) and populates INTO THE CHILD'S OWN real MultiState -
      ! never into `this`'s own `multi_state` argument. The child's own
      ! graph/component code requires no awareness of its parent for
      ! this to happen; only the parent reaches down.
      n = this%get_num_children()
      do i = 1, n
         child_name = this%get_child_name(i, _RC)
         child_spec => this%get_child_component_spec(child_name, _RC)
         child_graph => this%get_child_component_graph(child_name, _RC)
         child_meta => this%get_child_outer_meta(child_name, _RC)
         child_driver => child_meta%get_user_gc_driver()
         child_states = child_driver%get_states()
         associate (e => child_spec%var_specs%ftn_end())
            iter = child_spec%var_specs%ftn_begin()
            do while (iter /= e)
               call iter%next()
               var_spec => iter%of()
               if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT .or. &
                    var_spec%state_intent == ESMF_STATEINTENT_EXPORT)) cycle
               id_ptr => child_graph%get_resource_index(item_key(var_spec%state_intent, var_spec%short_name))
               call populate_one(graph, net, child_states, id_ptr, var_spec%state_intent, var_spec%short_name, _RC)
            end do
         end associate
      end do

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_populate_state

   ! Shared by both walks in graphbuilder_populate_state above: resolves
   ! id_ptr's own real field (export: direct; import: predecessor walk)
   ! and, if found, inserts a NAMED ALIAS of it (not the raw field
   ! itself) into multi_state's own real ESMF_State (per state_intent),
   ! under short_name.
   !
   ! openspec/changes/graph-child-state-population, found during
   ! implementation (ESMF_StateAddReplace matches purely by name - see
   ! this procedure's own earlier revision's header comment, now
   ! corrected below): the chain's final item (a mismatch/extension
   ! case) is created with NO explicit name
   ! (try_materialize_field_extension's own mismatch-branch call site
   ! never passes `name=`, confirmed by reading GraphBuilder.F90's
   ! own resolve_one) - ESMF assigns such a field some internal default
   ! name of its own. Inserting that raw, unnamed field directly meant
   ! GENERIC_INIT_PHASE_SEQUENCE's second GENERIC_INIT_ACCEPT_TRANSFER
   ! pass (enums/GenericPhases.F90's own documented fixed two-pass
   ! schedule) could not reliably re-match its own first-pass insertion
   ! by name, producing a real duplicate entry rather than a replace -
   ! ESMF objects are shallow and the underlying field is genuinely the
   ! same handle both times (materialize_field_extension's own "already
   ! materialized - no-op" guard, confirmed unmodified), but the STATE
   ! ENTRY identity ESMF tracks is name-based, not object-identity-based.
   ! Legacy's own FieldClassAspect%add_to_state
   ! (superstructure/generic/specs/FieldClassAspect.F90) already solves
   ! this exact problem the correct way: never add the raw field -
   ! always add a MAPL_NamedAlias of it (infrastructure/field/
   ! FieldNamedAlias.F90), under the item's own known short_name, so
   ! every insertion of the same underlying item - first pass, second
   ! pass, or any later re-entrant call - produces an alias with the
   ! identical, stable name ESMF_StateAddReplace can always match
   ! against. Reused here verbatim, not reinvented: short_name is now
   ! threaded through from each call site's own already-available
   ! var_spec%short_name (both graphbuilder_populate_state loops above
   ! already have it in scope).
   module subroutine populate_one(graph, net, multi_state, id_ptr, state_intent, short_name, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      type(MultiState), intent(inout) :: multi_state
      type(NodeId), pointer, intent(in) :: id_ptr
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(NodeId) :: item_id, field_node_id
      logical :: found
      type(ESMF_Field) :: field
      type(ESMF_Field) :: alias
      type(ESMF_State) :: state
      type(RestartMode), allocatable :: restart_mode
      type(ESMF_Info) :: info
      integer :: alias_id

      if (.not. associated(id_ptr)) then
         _RETURN(_SUCCESS)
      end if
      item_id = id_ptr

      if (state_intent == ESMF_STATEINTENT_EXPORT) then
         call get_field_if_materialized(graph, item_id, field, found, _RC)
      else
         field_node_id = find_materialized_predecessor(graph, net, item_id, found)
         ! Bug found while debugging this sub-change's own pFUnit
         ! coverage: a single-statement conditional guarding a call
         ! whose trailing argument is the error-handling macro does NOT
         ! guard that macro's own trailing verify-status check (a
         ! textually separate statement after the semicolon it expands
         ! to) - when found is false, the call itself is skipped but the
         ! verify check still runs unconditionally against an
         ! uninitialized status value, producing a spurious failure. A
         ! real if-then-end-if block keeps the whole macro expansion
         ! inside the conditional.
         if (found) then
            call get_field_if_materialized(graph, field_node_id, field, found, _RC)
         end if
      end if
      if (.not. found) then
         _RETURN(_SUCCESS)
      end if

      call multi_state%get_state(state, state_intent, _RC)

      ! Named alias under short_name, then add-or-replace by that same
      ! stable name - idempotent on re-entry (GENERIC_INIT_ACCEPT_TRANSFER's
      ! own two-pass schedule) because the alias's own name never
      ! changes across calls, regardless of whether the underlying
      ! field itself was ever given an explicit name at FieldCreate
      ! time (design.md's own update records this correction).
      alias = MAPL_NamedAlias(field, name=short_name, _RC)
      call ESMF_StateAddReplace(state, [alias], _RC)

      ! openspec/changes/phase-13-restart-compatibility (docs/graph/spec/
      ! 20-implementation-roadmap.md sec 20.4.13): restart_mode is
      ! per-connection-endpoint metadata, keyed by THIS alias's own id -
      ! same FieldInfoSetInternal mechanism and alias-id keying as
      ! legacy's FieldClassAspect%add_to_state
      ! (FieldClassAspect.F90:493-498). Read off item_id's OWN declared
      ! payload (the import's or export's own item - whichever id_ptr
      ! is), never field_node_id's: mirrors legacy's own
      ! connect_to_export, which does NOT mirror restart_mode from the
      ! export side onto an import's FieldClassAspect (only long_name/
      ! fill_value are mirrored there) - each declared item keeps its
      ! own restart_mode, independent of which real field backs it.
      call get_declared_restart_mode(graph, item_id, restart_mode)
      if (allocated(restart_mode)) then
         call ESMF_NamedAliasGet(alias, id=alias_id, _RC)
         call ESMF_InfoGetFromHost(alias, info, _RC)
         call FieldInfoSetInternal(info, alias_id, restart_mode, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine populate_one

   ! Field-only, no-op-if-not-materialized accessor shared by
   ! graphbuilder_populate_state's export and (resolved) import cases.
   module subroutine get_field_if_materialized(graph, node_id, field, found, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(ESMF_Field), intent(out) :: field
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status
      class(GraphNode), pointer :: node
      type(GraphStateItem) :: payload
      type(ESMF_StateItem_Flag) :: kind_flag

      found = .false.
      node => graph%get_node(node_id)
      if (.not. associated(node)) then
         _RETURN(_SUCCESS)
      end if

      select type (node)
      class is (StateItemNode)
         payload = node%get_payload()
         kind_flag = payload%itemType(_RC)
         if (kind_flag == ESMF_STATEITEM_FIELD) then
            field = payload%get_field(_RC)
            found = .true.
         end if
      end select

      _RETURN(_SUCCESS)
   end subroutine get_field_if_materialized

   ! openspec/changes/phase-13-restart-compatibility: populate_one's own
   ! helper - reads node_id's own StateItemNode payload's declared
   ! restart_mode (set at advertise time by populate_characteristics,
   ! GraphBuilder_Advertising_smod.F90), if any. Not a module procedure
   ! (no GraphBuilder.F90 interface declaration): private to this
   ! submodule, used only by populate_one above.
   subroutine get_declared_restart_mode(graph, node_id, restart_mode)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(RestartMode), allocatable, intent(out) :: restart_mode

      class(GraphNode), pointer :: node

      node => graph%get_node(node_id)
      if (.not. associated(node)) return

      select type (node)
      class is (StateItemNode)
         block
            type(GraphStateItem) :: payload
            payload = node%get_payload()
            call payload%get_declared_restart_mode(restart_mode)
         end block
      end select
   end subroutine get_declared_restart_mode

   ! Walks network's own predecessor adjacency from start_id, one hop at
   ! a time, until a node whose own payload is a real, allocated field is
   ! reached (design.md "State-population import resolution walks the
   ! dependency network's own predecessor edge, not a parallel
   ! bookkeeping structure"). Collapses to a single hop for a direct
   ! exact-match wiring (start_id's own predecessor IS the materialized
   ! export) and walks through an arbitrary-length extension chain the
   ! same way, with no special-casing between the two cases. Takes the
   ! FIRST predecessor at each hop - safe because add_dependency/
   ! graphbuilder_freeze's own existing "two producers for the same
   ! item" validation already guarantees single-predecessor by
   ! construction for any import this procedure is ever called on
   ! (design.md Risks). Returns found=.false. (not an error) if no
   ! predecessor exists at all (an unresolved import) or the walk
   ! exhausts a defensive hop limit without finding a real field.
   module function find_materialized_predecessor(graph, net, start_id, found) result(field_node_id)
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      type(NodeId), intent(in) :: start_id
      logical, intent(out) :: found
      type(NodeId) :: field_node_id

      integer, parameter :: MAX_CHAIN_HOPS = 64
      type(NodeId) :: current
      class(GraphNode), pointer :: node
      type(GraphStateItem) :: payload
      type(ESMF_StateItem_Flag) :: kind_flag
      ! target required: ftn_begin()'s own returned iterator holds a
      ! pointer into preds' internals (single_predecessor's own
      ! precedent, Test_GraphBuilderEquivalence.pf).
      type(NodeIdSet), target :: preds
      type(NodeIdSetIterator) :: pred_iter
      integer :: hop, status

      found = .false.
      current = start_id

      do hop = 1, MAX_CHAIN_HOPS
         node => graph%get_node(current)
         if (.not. associated(node)) exit

         select type (node)
         class is (StateItemNode)
            payload = node%get_payload()
            kind_flag = payload%itemType(rc=status)
            if (status == 0 .and. kind_flag == ESMF_STATEITEM_FIELD) then
               field_node_id = current
               found = .true.
               return
            end if
         end select

         if (.not. net%has_predecessors(current)) exit
         preds = net%get_predecessors(current)
         pred_iter = preds%ftn_begin()
         call pred_iter%next()
         current = pred_iter%of()
      end do
   end function find_materialized_predecessor

end submodule GraphBuilder_StatePopulation_smod
