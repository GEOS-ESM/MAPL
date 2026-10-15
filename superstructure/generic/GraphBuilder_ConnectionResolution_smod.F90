#include "MAPL.h"

! Task 3: Ordinary connection resolution
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)
!
! openspec/changes/graph-builder-stateitem-resolution: identity
! (short_name/state_intent/is_inout_borrower/callback_interface_id) and
! mismatch-characteristics (units/typekind/vertical_coordinate) are read
! directly off each already-advertised StateItemNode/GraphStateItem
! (set once by advertise_one, GraphBuilder_Advertising_smod.F90), never
! by re-reading a VariableSpec reached by component name -
! for_each_matching_import iterates the destination ComponentGraph's own
! import-port table (not dst_spec%var_specs); build_characteristics is
! removed (every call site reads GraphStateItem%get_characteristics()
! off the resolved NodeId's own node); find_export_var_spec/
! find_inout_borrower_var_spec are removed (both become an ordinary
! port-table scan over nodes' own short_name/is_inout_borrower fields).
submodule (mapl_GraphBuilder_mod) GraphBuilder_ConnectionResolution_smod
   use esmf, only: ESMF_State, ESMF_StateGet, ESMF_STATEITEM_NOTFOUND, operator(==)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator, operator(/=)
   use mapl_ExtensionResolution_mod, only: find_mismatched_characteristics, find_or_build_extension_chain, materialize_extensions_enabled
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_REGISTRY
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_PortIdNodeIdMap_mod, only: PortIdNodeIdMap, PortIdNodeIdMapIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Read-only counterpart of resolve_match_connection() below: same
   ! destination-import filtering and same-name export existence check,
   ! but never calls resolve_local_node_id() or add_dependency() -
   ! no proxy node or dependency edge is created here.
   module subroutine check_match_connection_unsatisfied(this, conn, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(OuterMetaComponent), pointer :: dst_meta, src_meta
      type(ComponentGraph), pointer :: dst_graph

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      ! openspec/changes/graph-builder-resolution-index: resolved via
      ! resolve_qualified_component (reaching any descendant, not only
      ! "<self>" or a direct child) rather than component_spec_for's own
      ! single-level dispatch - see
      ! GraphBuilder_ConnectionResolution_smod.F90's module header for
      ! this capability's scope.
      dst_meta => resolve_qualified_component(this, dst_pt%component_name, _RC)
      dst_graph => dst_meta%get_component_graph()
      src_meta => resolve_qualified_component(this, src_pt%component_name, _RC)

      call for_each_matching_import(dst_graph, dst_pt, check_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine check_one(node, rc)
         class(StateItemNode), target, intent(in) :: node
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_export
         type(CallbackInterfaceId) :: callback_interface_id
         type(QualifiedExportEntry), allocatable :: callback_matches(:)

         ! openspec/changes/callback-wiring, design.md Decision 0: a
         ! destination declaring an expected callback interface is
         ! resolved against the flattened namespace instead of exact-
         ! name matching - read-only here (resolve_callback_import never
         ! mutates the graph), matching this procedure's own activate-
         ! time contract.
         !
         ! NAG rejects chaining a method call directly on another
         ! function call's own result (`f()%method()`) - a local
         ! temporary is required (same throughout this file).
         callback_interface_id = node%get_callback_interface_id()
         if (callback_interface_id%is_valid()) then
            callback_matches = resolve_callback_import(this, src_pt, callback_interface_id, rc=status)
            _VERIFY(status)
            if (size(callback_matches) == 0) then
               call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, node%get_short_name()))
            end if
            _RETURN(_SUCCESS)
         end if

         has_export = associated(find_export_node(src_meta, node%get_short_name()))
         if (.not. has_export) then
            call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, node%get_short_name()))
         end if

         _RETURN(_SUCCESS)
      end subroutine check_one

   end subroutine check_match_connection_unsatisfied

   ! Mirrors MatchConnection%activate()'s own two-step structure
   ! (superstructure/generic/connection/MatchConnection.F90) - filter the
   ! destination's imports against the declared destination pattern, then
   ! for each match require an EXACT-name export on the source side -
   ! but recomputes it directly against the destination's own
   ! ComponentGraph import-port table (design.md Decisions: "reuses
   ! MatchConnection's semantics, not its implementation") rather than
   ! going through StateRegistry's subregistry/family machinery.
   ! VirtualConnectionPt%matches() and the ConnectionPt/
   ! VirtualConnectionPt read-only accessors are ordinary public query
   ! methods on the Connection's own declared data, not part of that
   ! execution machinery, and are used here as-is.
   module subroutine resolve_match_connection(this, conn, unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(OuterMetaComponent), pointer :: dst_meta
      type(ComponentGraph), pointer :: dst_graph
      type(ComponentGraph), pointer :: this_graph
      type(DependencyNetworkId) :: net_id

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      ! openspec/changes/graph-builder-resolution-index: see
      ! check_match_connection_unsatisfied's own identical replacement
      ! above - resolve_qualified_component in place of
      ! component_spec_for's own single-level dispatch.
      dst_meta => resolve_qualified_component(this, dst_pt%component_name, _RC)
      dst_graph => dst_meta%get_component_graph()

      this_graph => this%get_component_graph()
      net_id = this_graph%get_default_network_id()

      call for_each_matching_import(dst_graph, dst_pt, resolve_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine resolve_one(dst_node, rc)
         class(StateItemNode), target, intent(in) :: dst_node
         integer, optional, intent(out) :: rc

         integer :: status
         type(NodeId) :: import_node_id, export_node_id, final_node_id
         type(NodeId), allocatable :: intermediate_node_ids(:)
         logical :: has_export
         ! openspec/changes/phase-11a-export-field-aliasing: set by
         ! try_alias_export_field at both of this procedure's own
         ! export_node_id materialization call sites below - when
         ! true, the alias already gives export_node_id a real field,
         ! so the existing try_materialize_field_extension call at that
         ! same site is skipped (D4).
         logical :: alias_found
         class(StateItemNode), pointer :: export_node
         type(GraphStateItem), pointer :: export_item, import_item
         type(StateItemCharacteristicKind), allocatable :: mismatched(:)
         character(:), allocatable :: unsupported_characteristic
         character(:), allocatable :: materialization_failure
         type(StringVector) :: callback_rejected
         character(:), pointer :: rejected_item
         integer :: j
         type(CallbackInterfaceId) :: callback_interface_id

         ! openspec/changes/ordinary-inout-direct-alias: a destination
         ! declaring BOTH an expected callback interface and ordinary
         ! inout borrower intent is a nonsensical combination - two
         ! separate, mutually exclusive resolution paths exist below
         ! (callback-interface branch, then inout-borrower branch), and
         ! resolving via whichever branch happens to run first would
         ! silently ignore the other declaration rather than honoring it.
         ! Rejected explicitly, checked before either branch, so neither
         ! one ever silently wins.
         callback_interface_id = dst_node%get_callback_interface_id()
         if (callback_interface_id%is_valid() .and. dst_node%is_inout_borrower()) then
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                 'declares both an expected callback interface and inout borrower intent (mutually exclusive)'))
            _RETURN(_SUCCESS)
         end if

         ! openspec/changes/callback-wiring, design.md Decision 0: a
         ! destination declaring an expected callback interface is
         ! resolved against the flattened namespace, materialized into
         ! its own collection, and wired to the import via an ordinary
         ! add_dependency edge (resolve_callback_destination) - instead
         ! of the exact-name/extension-chain logic below, which never
         ! runs for this item.
          if (callback_interface_id%is_valid()) then
             call resolve_callback_destination(this, src_pt, dst_pt, dst_node%get_short_name(), &
                  callback_interface_id, callback_rejected, _RC)
             do j = 1, callback_rejected%size()
                rejected_item => callback_rejected%of(j)
                call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                     rejected_item))
             end do
             _RETURN(_SUCCESS)
          end if

          ! openspec/changes/ordinary-inout-direct-alias, design.md
          ! Decisions 1/4: a destination declared as an ordinary inout
          ! borrower is resolved by resolve_inout_destination instead of
          ! the exact-name/extension-chain logic below, which never runs
          ! for this item - mirrors the callback-interface branch
          ! immediately above exactly.
          if (dst_node%is_inout_borrower()) then
             call resolve_inout_destination(this, src_pt, dst_pt, dst_node, unsupported, _RC)
             _RETURN(_SUCCESS)
          end if

          import_node_id = resolve_local_node_id(this, dst_pt%component_name, &
              ESMF_STATEINTENT_IMPORT, dst_node%get_short_name(), _RC)

         has_export = find_export_node_id(this, src_pt%component_name, dst_node%get_short_name(), &
              export_node_id, export_node, _RC)

         if (.not. has_export) then
            ! REQ scenario "Import with no matching export is left
            ! unresolved" - reported, not silently dropped.
            call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name()))
            _RETURN(_SUCCESS)
         end if

         export_item => export_node%get_payload_ptr()
         import_item => dst_node%get_payload_ptr()
         mismatched = find_mismatched_characteristics(export_item%get_characteristics(), import_item%get_characteristics())

         if (size(mismatched) == 0) then
            ! REQ-EXT-003 no-op case: matches exactly, wire directly.
            call this_graph%add_dependency(net_id, export_node_id, import_node_id, _RC)

            ! openspec/changes/phase-10a-state-materialization: give the
            ! export's own node a real, allocated field - the same
            ! Field-only/geom/vgrid resolution the mismatch branch below
            ! already uses via try_materialize_field_extension, reused
            ! as-is (design.md "Reuse, don't duplicate the mismatch
            ! branch's own geom/vgrid resolution"), gated on this
            ! sub-change's own mode selector (not
            ! materialize_extensions_enabled() - a different, orthogonal
            ! gate owned by the extension-reuse capability). No-op (zero
            ! behavior change) in GRAPH_MODE_REGISTRY, this sub-change's
            ! default.
            if (graph_mode() /= GRAPH_MODE_REGISTRY) then
               ! openspec/changes/phase-11a-export-field-aliasing: try
               ! the real alias first (D4) - if it succeeds, the node
               ! already has a real field, same as comp_src's own real
               ! export field; try_materialize_field_extension's own
               ! FieldCreate path below is skipped entirely (calling it
               ! afterward would silently overwrite the alias, defeating
               ! the whole point).
               call try_alias_export_field(this, src_pt%component_name, export_node%get_short_name(), &
                    this_graph, export_node_id, alias_found, _RC)
               if (.not. alias_found) then
                  call try_materialize_field_extension(this, src_pt%component_name, export_item, import_item, &
                       this_graph, export_node_id, materialization_failure, name=export_node%get_short_name(), _RC)
                  if (materialization_failure /= '') then
                     ! Same distinguishable-failure reporting the mismatch
                     ! branch below already uses - the edge above still
                     ! exists (wiring succeeded; only payload materialization
                     ! did not).
                     call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                          materialization_failure))
                  end if
               end if
            end if

            _RETURN(_SUCCESS)
         end if

         ! REQ-EXT-001/005: mismatch - delegate to the extension-reuse
         ! capability (mapl_ExtensionResolution_mod) rather than wiring
         ! the mismatched pair directly.
         call find_or_build_extension_chain(this_graph, net_id, export_node_id, &
              export_item%get_characteristics(), import_item%get_characteristics(), mismatched, &
              final_node_id, intermediate_node_ids, unsupported_characteristic, _RC)

         if (unsupported_characteristic /= '') then
            ! spec "Unregistered characteristic fails loudly" -
            ! distinguishable from "import has no matching export."
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                 unsupported_characteristic))
            _RETURN(_SUCCESS)
         end if

         ! openspec/changes/graph-child-state-population, found during
         ! implementation (this change's own YAML-configured
         ! demonstration scenario): the mismatch branch's own
         ! export_node_id (the chain's own INPUT) was never materialized
         ! by anything - only the exact-match branch above and the
         ! chain's own FINAL node (below) ever call
         ! try_materialize_field_extension. Harmless as long as nothing
         ! ever executes the chain's own compute() for real (no fixture
         ! did, before this change's own run-phase pull, design.md D7);
         ! UnitsConverterTransform%compute()'s own
         ! `input_field = input_item%get_field(_RC)` call fails outright
         ! on an unallocated input otherwise. Fixed by materializing
         ! export_node_id here too, using the EXPORT's own typekind/units/
         ! geom/vgrid - NOT the import's (passing export_item for BOTH
         ! the export and import arguments is what makes
         ! try_materialize_field_extension's own "always use the second
         ! argument's typekind/units" contract resolve to the export's
         ! own values here, correct for the chain's own input node, as
         ! opposed to the chain's final node below, which correctly
         ! keeps using the import's own values). Gated the same way the
         ! exact-match branch's own materialization already is
         ! (graph_mode() /= GRAPH_MODE_REGISTRY) - a no-op in
         ! GRAPH_MODE_REGISTRY, matching every other new code path this
         ! change adds.
         if (graph_mode() /= GRAPH_MODE_REGISTRY) then
            ! openspec/changes/phase-11a-export-field-aliasing: same
            ! guard, same skip-when-found treatment as the exact-match
            ! branch above (D4) - no divergence between the two call
            ! sites.
            call try_alias_export_field(this, src_pt%component_name, export_node%get_short_name(), &
                 this_graph, export_node_id, alias_found, _RC)
            if (.not. alias_found) then
               call try_materialize_field_extension(this, src_pt%component_name, export_item, export_item, &
                    this_graph, export_node_id, materialization_failure, name=export_node%get_short_name(), _RC)
               if (materialization_failure /= '') then
                  call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                       materialization_failure))
                  _RETURN(_SUCCESS)
               end if
            end if
         end if

         ! extension-registry-visibility change, task groups 1-3: chain
         ! *structure* above is unconditional (unaffected by the gate,
         ! exactly as 3c left it) - only the real FieldCreate-based
         ! payload materialization step is gated. Off (the default): the
         ! extension item keeps 3c's own unallocated placeholder payload,
         ! zero behavior change from today.
         !
         ! openspec/changes/phase-11b-chain-intermediate-materialization,
         ! design.md D3/D4: replaces the old single final-node
         ! try_materialize_field_extension call with
         ! materialize_extension_chain, which materializes every entry
         ! of intermediate_node_ids (every link, not only the chain's
         ! own final node) with each link's own correctly-evolved
         ! typekind/units. Reduces to the old single-call behavior for a
         ! single-characteristic mismatch (intermediate_node_ids has
         ! length 1, equal to final_node_id).
         if (materialize_extensions_enabled()) then
            call materialize_extension_chain(this, src_pt%component_name, export_item, import_item, &
                 mismatched, intermediate_node_ids, this_graph, materialization_failure, _RC)
            if (materialization_failure /= '') then
               ! Distinguishable from both "no matching export" and
               ! "unregistered characteristic fails loudly" above (spec
               ! "A non-field item class fails explicitly").
               call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
                    materialization_failure))
               _RETURN(_SUCCESS)
            end if
         end if

         call this_graph%add_dependency(net_id, final_node_id, import_node_id, _RC)

         _RETURN(_SUCCESS)
      end subroutine resolve_one

   end subroutine resolve_match_connection

   ! GENERIC_INIT_ADVERTISE-time analog of legacy Connection%activate()
   ! (module header - "Two-phase timing"): for each MatchConnection,
   ! determine which ordinary-match destination imports have a matching
   ! source export, WITHOUT creating any graph structure (no proxy
   ! nodes, no dependency edges - those are graphbuilder_resolve_connections'
   ! job, at connect() time). Read-only with respect to the graph;
   ! purely populates the unresolved-imports report, which is what
   ! legacy's own propagate_unsatisfied_imports() decision (bubble an
   ! unsatisfied import to the parent) is based on at this same phase.
   module subroutine graphbuilder_check_unsatisfied_imports(this, unusable, unresolved_imports, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved_imports
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(GraphResolutionEntryVector) :: unresolved

      comp_spec => this%get_component_spec()

      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               call check_match_connection_unsatisfied(this, c, unresolved, _RC)
            class default
               ! Wildcard/callback/reexport/simple connections: out of
               ! scope for this slice - deliberately skipped.
            end select
         end do
      end associate

      if (present(unresolved_imports)) unresolved_imports = unresolved

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_check_unsatisfied_imports

   ! Shared by check_match_connection_unsatisfied() and
   ! resolve_match_connection() below: both used to independently repeat
   ! this same "which of dst_graph's import ports does dst_pt's declared
   ! pattern match" scan (module header - "same destination-import
   ! filtering"). Owns the loop and both guard clauses; each caller
   ! supplies only the per-match step, as an internal procedure so it can
   ! host-associate whatever extra context it needs (this, src_pt,
   ! unresolved, ...) - same style as OuterMetaComponent's own
   ! apply_to_children_custom()/I_child_op.
   !
   ! openspec/changes/graph-builder-stateitem-resolution, design.md
   ! Decisions: iterates dst_graph's own import-port table
   ! (ComponentGraph%get_import_ports(), new - tasks.md 3.1 design.md
   ! correction) instead of dst_spec%var_specs - for each bound import
   ! PortId, the corresponding StateItemNode is retrieved
   ! (graph%get_node(node_id), already public) and its own
   ! get_short_name()/get_state_intent() fields (new, task 1.1) are used
   ! for the dst_pt%v_pt%matches(...) filter - structurally the same
   ! filter condition as before, just sourced from the node instead of a
   ! var_spec. op's own signature is `subroutine(node, rc)` (task 3.1).
   module subroutine for_each_matching_import(dst_graph, dst_pt, op, rc)
      type(ComponentGraph), target, intent(in) :: dst_graph
      type(ConnectionPt), intent(in) :: dst_pt
      procedure(I_match_op) :: op
      integer, optional, intent(out) :: rc

      integer :: status
      type(PortIdNodeIdMap), target :: import_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: node_id
      class(GraphNode), pointer :: generic_node

      import_ports = dst_graph%get_import_ports()

      associate (e => import_ports%ftn_end())
         iter = import_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            node_id = iter%second()
            generic_node => dst_graph%get_node(node_id)
            _ASSERT(associated(generic_node), 'GraphBuilder: for_each_matching_import - import port NodeId not found')
            select type (generic_node)
            class is (StateItemNode)
               if (.not. (generic_node%get_state_intent() == ESMF_STATEINTENT_IMPORT)) cycle
               if (.not. dst_pt%v_pt%matches(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, generic_node%get_short_name()))) cycle
               call op(generic_node, _RC)
            class default
               _FAIL('GraphBuilder: for_each_matching_import - import port NodeId is not a StateItemNode')
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine for_each_matching_import

   ! GENERIC_INIT_ACCEPT_TRANSFER-time analog of legacy
   ! Connection%connect() (module header - "Two-phase timing"): the real
   ! graph mutation - proxy nodes and dependency edges - restricted to
   ! MatchConnection (see module header). Any other declared Connection
   ! subtype (SimpleConnection, ReexportConnection, ...) is left
   ! untouched - out of scope for this slice, not an oversight
   ! (proposal.md "Explicitly out of scope"). REQ-GB-003 "Resolving
   ! connections" / "Creating dependency networks".
   !
   ! Idempotency: ESMF's Provide/Accept/Realize transfer negotiation can
   ! invoke GENERIC_INIT_ACCEPT_TRANSFER (and hence this) more than once
   ! per component within a single Initialize sequence - legacy's own
   ! MatchConnection%connect()/SimpleConnection%connect()
   ! (superstructure/generic/connection/ *.F90) guard against exactly this
   ! with a per-connection `consumed` flag. GraphBuilder has no equivalent
   ! per-connection state to reuse, so it guards at the graph level
   ! instead: once this component's graph is frozen, every ordinary
   ! connection it declares has already been resolved, so a repeat call
   ! is a safe no-op rather than an attempt to mutate a frozen graph.
    module subroutine graphbuilder_resolve_connections(this, unusable, unresolved_imports, &
         unsupported_characteristics, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved_imports
      ! REQ scenario "Unregistered characteristic fails loudly": a
      ! distinguishable report, separate from unresolved_imports (which
      ! means "no export at all"), for a matched export/import pair whose
      ! mismatch has no registered extension provider
      ! (mapl_ExtensionResolution_mod).
      type(GraphResolutionEntryVector), optional, intent(out) :: unsupported_characteristics
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: graph
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(GraphResolutionEntryVector) :: unresolved
      type(GraphResolutionEntryVector) :: unsupported

      graph => this%get_component_graph()
      if (graph%is_frozen()) then
         if (present(unresolved_imports)) unresolved_imports = unresolved
         if (present(unsupported_characteristics)) unsupported_characteristics = unsupported
         _RETURN(_SUCCESS)
      end if

      comp_spec => this%get_component_spec()

      associate (e => comp_spec%connections%ftn_end())
         iter = comp_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               call resolve_match_connection(this, c, unresolved, unsupported, _RC)
            class default
               ! Wildcard/callback/reexport/simple connections: out of
               ! scope for this slice - deliberately skipped.
            end select
         end do
      end associate

      if (present(unresolved_imports)) unresolved_imports = unresolved
      if (present(unsupported_characteristics)) unsupported_characteristics = unsupported

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine graphbuilder_resolve_connections

   ! openspec/changes/graph-builder-stateitem-resolution, tasks.md 4.2:
   ! finds the export's own NodeId/StateItemNode by scanning the source
   ! ComponentGraph's own export-port table for a node whose
   ! get_short_name() matches, instead of scanning src_spec%var_specs
   ! (the now-removed find_export_var_spec's own job) - the single
   ! replacement for both find_export_var_spec and the NodeId half this
   ! function already performed. export_node is an output argument
   ! (StateItemNode, by pointer) instead of a VariableSpec, since callers
   ! need the node's fields now, not a var_spec's.
   ! openspec/changes/graph-builder-stateitem-resolution, tasks.md 4.2:
   ! read-only scan of src_meta's own ComponentGraph export-port table
   ! for a node whose get_short_name() matches - the direct node-based
   ! replacement for the now-removed find_export_var_spec, used by
   ! find_export_node_id below (which additionally mutates the graph via
   ! resolve_local_node_id) AND by check_match_connection_unsatisfied's
   ! own check_one (which must NOT create graph structure - this
   ! procedure's own module-header "activate()-time analog" contract).
   function find_export_node(src_meta, short_name) result(export_node)
      class(OuterMetaComponent), target, intent(inout) :: src_meta
      character(*), intent(in) :: short_name
      class(StateItemNode), pointer :: export_node

      type(ComponentGraph), pointer :: src_graph
      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: candidate_id
      class(GraphNode), pointer :: candidate_node

      export_node => null()

      src_graph => src_meta%get_component_graph()
      export_ports = src_graph%get_export_ports()

      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate_id = iter%second()
            candidate_node => src_graph%get_node(candidate_id)
            if (.not. associated(candidate_node)) cycle
            select type (candidate_node)
            class is (StateItemNode)
               if (candidate_node%get_short_name() == short_name) then
                  export_node => candidate_node
                  return
               end if
            class default
               ! Not a StateItemNode (e.g. a different GraphNode kind
               ! co-owned by this graph) - not an export candidate,
               ! skipped rather than failing.
            end select
         end do
      end associate
   end function find_export_node

   logical module function find_export_node_id(this, comp_name, short_name, export_node_id, export_node, rc) result(found)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      character(*), intent(in) :: short_name
      type(NodeId), intent(out) :: export_node_id
      class(StateItemNode), pointer, intent(out) :: export_node
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: src_meta

      ! openspec/changes/graph-builder-resolution-index: see
      ! check_match_connection_unsatisfied's own identical replacement
      ! above - resolve_qualified_component in place of
      ! component_spec_for's own single-level dispatch. The export's
      ! real NodeId (resolve_local_node_id below) already reaches
      ! any descendant level via the same primitive, same change.
      src_meta => resolve_qualified_component(this, comp_name, _RC)
      export_node => find_export_node(src_meta, short_name)
      found = associated(export_node)

      if (.not. found) then
         _RETURN(_SUCCESS)
      end if

      export_node_id = resolve_local_node_id(this, comp_name, ESMF_STATEINTENT_EXPORT, short_name, _RC)

      _RETURN(_SUCCESS)
   end function find_export_node_id

   ! openspec/changes/ordinary-inout-direct-alias, design.md Decisions
   ! 1-4: resolves a destination item declared an ordinary inout borrower
   ! (16-inout-items.md REQ-INOUT-001, direct-alias case only). Reuses
   ! find_export_node_id/find_mismatched_characteristics exactly as
   ! resolve_one's own ordinary (non-inout) path does for the REQ-EXT-003
   ! no-op comparison - owner and borrower come to share one underlying
   ! payload through the same materialization path ordinary no-op
   ! connections already use, since the forward edge below is wired
   ! identically. On an exact match, also adds a return-network edge
   ! (borrower -> owner) in a fresh per-pairing network (Decision 2,
   ! revised - mirrors callback wiring's own per-binding
   ! create_network() precedent) so the pairing satisfies REQ-DEP-008a by
   ! construction (forward writes the borrower's NodeId, return writes
   ! the owner's NodeId - two different ids, never the same network).
   ! Every shape REQ-INOUT-002 reserves for a future design addendum
   ! (mismatched payload, no identifiable owner, chained/recursive
   ! borrowing) is rejected here and reported via `unsupported`, never
   ! delegated to extension-chain construction. Decision 3 (revised):
   ! this subroutine lands graph structure only - the runtime return-edge
   ! propagation trigger is explicitly deferred (no borrower-execution-
   ! completion hook exists yet to attach it to).
   !
   ! openspec/changes/graph-builder-stateitem-resolution: dst_node (the
   ! borrower's own already-advertised StateItemNode) replaces the
   ! borrower's var_spec throughout.
   module subroutine resolve_inout_destination(this, src_pt, dst_pt, dst_node, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), target, intent(in) :: dst_node
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(DependencyNetworkId) :: net_id, return_net_id
      type(NodeId) :: import_node_id, owner_node_id
      logical :: has_owner
      class(StateItemNode), pointer :: owner_node
      type(ComponentGraph), pointer :: owner_graph
      type(OuterMetaComponent), pointer :: owner_meta
      type(PortIdNodeIdMap), target :: owner_import_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: candidate_id
      class(GraphNode), pointer :: candidate_node
      logical :: owner_is_chained
      type(GraphStateItem), pointer :: owner_item, borrower_item
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)

      graph => this%get_component_graph()
      net_id = graph%get_default_network_id()

      import_node_id = resolve_local_node_id(this, dst_pt%component_name, &
           ESMF_STATEINTENT_IMPORT, dst_node%get_short_name(), _RC)

      has_owner = find_export_node_id(this, src_pt%component_name, dst_node%get_short_name(), &
           owner_node_id, owner_node, _RC)

      if (.not. has_owner) then
         ! spec "A borrower declaration with no identifiable owner is
         ! rejected" - reported, never silently treated as an ordinary
         ! import.
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
              'inout borrower has no identifiable owner'))
         _RETURN(_SUCCESS)
      end if

      ! spec "A chained borrowing pairing is rejected": owner_node
      ! (found above via find_export_node_id) is always the EXPORT-intent
      ! node on the source component - is_inout_borrower() is only ever
      ! true on an IMPORT-intent node (a borrower marks itself, not
      ! whoever re-exports its value), so checking
      ! owner_node%is_inout_borrower() directly would never detect
      ! chaining. Instead, check whether the source component itself
      ! separately declares a same-short-name IMPORT node marked an
      ! inout borrower - i.e. the value this destination would borrow is
      ! itself borrowed by its own owning component - via a scan over
      ! the owner's own import ports (the node-based replacement for the
      ! now-removed find_inout_borrower_var_spec, tasks.md 4.3).
      owner_meta => resolve_qualified_component(this, src_pt%component_name, _RC)
      owner_graph => owner_meta%get_component_graph()
      owner_import_ports = owner_graph%get_import_ports()
      owner_is_chained = .false.
      associate (e => owner_import_ports%ftn_end())
         iter = owner_import_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate_id = iter%second()
            candidate_node => owner_graph%get_node(candidate_id)
            _ASSERT(associated(candidate_node), 'GraphBuilder: resolve_inout_destination - import port NodeId not found')
            select type (candidate_node)
            class is (StateItemNode)
               if (candidate_node%get_short_name() == dst_node%get_short_name() .and. &
                    candidate_node%is_inout_borrower()) then
                  owner_is_chained = .true.
                  exit
               end if
            class default
               _FAIL('GraphBuilder: resolve_inout_destination - import port NodeId is not a StateItemNode')
            end select
         end do
      end associate
      if (owner_is_chained) then
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
              'inout borrower owner is itself a declared inout borrower (chained borrowing)'))
         _RETURN(_SUCCESS)
      end if

      owner_item => owner_node%get_payload_ptr()
      borrower_item => dst_node%get_payload_ptr()
      mismatched = find_mismatched_characteristics(owner_item%get_characteristics(), borrower_item%get_characteristics())

      if (size(mismatched) /= 0) then
         ! spec "A mismatched pairing is rejected rather than silently
         ! chained" - REQ-INOUT-002 reserves the non-direct-alias case;
         ! this capability MUST NOT delegate to extension-chain
         ! construction the way ordinary (non-inout) resolution does.
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, dst_node%get_short_name(), &
              'inout borrower payload does not exactly match its owner (non-direct-alias case not supported)'))
         _RETURN(_SUCCESS)
      end if

      ! REQ-INOUT-001 direct-alias case: forward edge in the default
      ! network (identical wiring to the ordinary REQ-EXT-003 no-op
      ! case), plus a return edge in a fresh per-pairing network - no
      ! TransformGraphNode for either edge.
      call graph%add_dependency(net_id, owner_node_id, import_node_id, _RC)

      return_net_id = graph%create_network(_RC)
      call graph%add_dependency(return_net_id, import_node_id, owner_node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_inout_destination

   ! openspec/changes/phase-11a-export-field-aliasing, design.md D1/D2/
   ! D3/D4/D5: materializes node_id's StateItemNode payload as a real
   ! MAPL_NamedAlias of the export component's own real, already-
   ! realized field - read directly off that component's own real
   ! export ESMF_State via a plain ESMF_StateGet (no StateRegistry
   ! involvement, D1) - instead of calling try_materialize_field_extension's
   ! own FieldCreate path. Tried BEFORE try_materialize_field_extension at
   ! both of resolve_one's own export_node_id call sites (D4): if
   ! found is true here, the caller skips try_materialize_field_extension
   ! entirely (the alias already gives the node a real field). If
   ! found is false (the export's own real field is not yet available
   ! by name - D2), this returns success, not an error, and the caller
   ! falls back to today's unchanged try_materialize_field_extension
   ! call.
   !
   ! openspec/changes/graph-builder-stateitem-resolution: export_var_spec
   ! replaced by export_short_name (a plain character argument) - this
   ! procedure only ever read %short_name off it, never any other field.
   module subroutine try_alias_export_field(this, export_comp_name, export_short_name, &
        graph, node_id, found, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      character(*), intent(in) :: export_short_name
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status
      class(OuterMetaComponent), pointer :: export_meta
      type(GriddedComponentDriver), pointer :: export_driver
      type(MultiState) :: export_states
      type(ESMF_State) :: export_state
      type(ESMF_StateItem_Flag) :: item_kind
      type(ESMF_Field) :: field
      type(ESMF_Field) :: alias
      class(GraphNode), pointer :: node
      type(GraphStateItem), pointer :: payload

      found = .false.

      export_meta => component_for(this, export_comp_name, _RC)
      export_driver => export_meta%get_user_gc_driver()
      export_states = export_driver%get_states()
      call export_states%get_state(export_state, ESMF_STATEINTENT_EXPORT, _RC)

      ! D1: plain ESMF-level by-name read - not a hard failure if the
      ! item is not yet present (D2's two-pass timing concern); checked
      ! via itemType, the same "present/absent" idiom
      ! FieldClassAspect%add_to_state already uses, rather than trusting
      ! ESMF_StateGet's own rc to distinguish "absent" from "present."
      call ESMF_StateGet(export_state, itemName=export_short_name, itemType=item_kind, _RC)
      if (item_kind == ESMF_STATEITEM_NOTFOUND) then
         _RETURN(_SUCCESS)
      end if

      call ESMF_StateGet(export_state, itemName=export_short_name, field=field, _RC)

      ! D5: MAPL_NamedAlias, not a hand-rolled ESMF_NamedAlias - carries
      ! MAPL's own per-alias-id metadata (standard_name/long_name/
      ! restart_mode), same as populate_one's own precedent.
      alias = MAPL_NamedAlias(field, name=export_short_name, _RC)

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'GraphBuilder: try_alias_export_field - NodeId not found in graph')
      select type (node)
      class is (StateItemNode)
         ! openspec/changes/hierarchy-wide-component-graph, found during
         ! implementation: with proxy nodes retired, node_id is now the
         ! real, already-advertised export node (characteristics already
         ! established by advertise_one), not a freshly-created,
         ! always-empty parent-local proxy. A wholesale set_payload()
         ! call here would discard that node's own established
         ! characteristics (typekind/units, read later by
         ! get_item_typekind/get_item_units) along with its payload -
         ! mutate the existing payload IN PLACE via get_payload_ptr()
         ! instead (same idiom populate_characteristics,
         ! GraphBuilder_Advertising_smod.F90, already uses for exactly
         ! this reason), so only the field handle changes.
         payload => node%get_payload_ptr()
         call payload%set(alias, _RC)
      class default
         _FAIL('GraphBuilder: try_alias_export_field - NodeId does not refer to a StateItemNode')
      end select

      found = .true.

      _RETURN(_SUCCESS)
   end subroutine try_alias_export_field

end submodule GraphBuilder_ConnectionResolution_smod
