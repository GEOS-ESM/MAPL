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
   use mapl_PortId_mod, only: PortId
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

          ! openspec/changes/hierarchy-wide-component-graph, tasks.md
          ! 6.3 (correction, discovered during that task): dst_node is
          ! already the exact match for_each_matching_import found by
          ! walking dst_graph's own import-port table - its own real
          ! NodeId (get_node_id(), BaseGraphNode's own accessor) is
          ! used directly instead of re-deriving the same id via
          ! resolve_local_node_id's own resource_index lookup.
          ! resource_index is populated only for a DIRECTLY-advertised
          ! item (advertise_one's own add_resource_index call); a
          ! MIRRORED import (graphbuilder_run_propagate_hook,
          ! GraphBuilder_ConnectionResolution_smod.F90 - task 6.1) is
          ! deliberately NOT given a resource_index entry (design.md
          ! D3's own text only calls add_import_port, never
          ! add_resource_index), so the old
          ! resolve_local_node_id(dst_pt%component_name, ...) call
          ! would hard-fail resolve_one's own "connection references an
          ! item never advertised" assertion for any mirrored import -
          ! contradicting the ADDED requirement's own stated scenario
          ! "An ordinary connection resolves a mirrored import exactly
          ! as it would an item advertised directly." Using dst_node's
          ! own id directly is strictly equivalent to the old lookup
          ! for every directly-advertised import too (both resolve to
          ! the same NodeId), so this is behavior-preserving, not a new
          ! resolution path.
          import_node_id = dst_node%get_node_id()

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

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.1,
   ! design.md D3 (corrected twice during implementation; see tasks.md
   ! 6.1's own correction note for the full history). "Resolved" cannot
   ! be read off dependency-edge wiring (no edge exists anywhere, for
   ! any component, at the ADVERTISE-time point this runs - edges are
   ! only added later, at ACCEPT_TRANSFER, by
   ! graphbuilder_resolve_connections) nor off
   ! graphbuilder_check_unsatisfied_imports(child_meta) alone (that
   ! procedure only ever inspects connections child_meta ITSELF
   ! declares; a connection declared on child_meta's own DIRECT PARENT
   ! naming child_meta directly - the single most common connection
   ! shape, e.g. an ordinary two-child connect_all - is invisible to
   ! it, which would wrongly report an already-resolved import as
   ! unresolved and risk a real two-producer REQ-DEP-008a violation
   ! once mirrored and later also independently resolved by some
   ! further ancestor's own connection naming this_parent). An import
   ! in child_meta's own import-port table counts as unresolved iff
   ! NEITHER (a) some MatchConnection declared ON child_meta itself,
   ! whose destination resolves (resolve_qualified_component) back to
   ! child_meta itself, NOR (b) some MatchConnection declared ON
   ! this_parent itself, whose destination resolves directly to
   ! child_meta - finds a matching source export for it (the same
   ! per-item positive check check_match_connection_unsatisfied's own
   ! check_one already makes - see connection_resolves_import below,
   ! shared by both cases). child_meta MUST be one of this_parent's own
   ! direct children. Read-only; creates no graph structure.
   module function graphbuilder_unresolved_import_node_ids(this_parent, child_meta, rc) result(unresolved)
      class(OuterMetaComponent), target, intent(inout) :: this_parent
      class(OuterMetaComponent), target, intent(inout) :: child_meta
      integer, optional, intent(out) :: rc
      type(UnresolvedImportEntry), allocatable :: unresolved(:)

      integer :: status
      type(ComponentGraph), pointer :: child_graph
      type(PortIdNodeIdMap), target :: import_ports
      type(PortIdNodeIdMapIterator) :: port_iter
      type(NodeId) :: node_id
      class(GraphNode), pointer :: generic_node
      logical :: resolved
      type(UnresolvedImportEntry) :: entry
      type(UnresolvedImportEntry), allocatable :: grown(:)
      integer :: n

      allocate(unresolved(0))
      child_graph => child_meta%get_component_graph()
      import_ports = child_graph%get_import_ports()

      associate (e => import_ports%ftn_end())
         port_iter = import_ports%ftn_begin()
         do while (port_iter /= e)
            call port_iter%next()
            node_id = port_iter%second()
            generic_node => child_graph%get_node(node_id)
            _ASSERT(associated(generic_node), 'GraphBuilder: unresolved_import_node_ids - import port NodeId not found')
            select type (generic_node)
            class is (StateItemNode)
               if (.not. (generic_node%get_state_intent() == ESMF_STATEINTENT_IMPORT)) cycle
               ! openspec/changes/hierarchy-wide-component-graph, tasks.md
               ! 6.8 (regression fix): an import with no graph-native
               ! resolution path at all (WILDCARD/SERVICE/EXPRESSION -
               ! see is_ordinary_resolvable_import's own header comment,
               ! GraphBuilder.F90) is excluded from propagation entirely
               ! - it is never "unresolved-and-propagatable" in the
               ! sense this scan means, since no ordinary ancestor
               ! connection could ever resolve it even once propagated.
               if (.not. is_ordinary_resolvable_import(generic_node)) cycle

               ! (a) resolved at child_meta's own level or below.
               resolved = connection_resolves_import(child_meta, child_meta, generic_node, _RC)
               ! (b) resolved by this_parent's own direct connection
               ! naming child_meta (the ordinary connect_all shape).
               if (.not. resolved) then
                  resolved = connection_resolves_import(this_parent, child_meta, generic_node, _RC)
               end if

               if (.not. resolved) then
                  entry%short_name = generic_node%get_short_name()
                  entry%node_id = node_id

                  ! Same self-referencing array-constructor ICE
                  ! precedent as collect_qualified_exports/
                  ! collect_qualified_export_index (growing via an
                  ! explicit fresh temporary + move_alloc).
                  n = size(unresolved)
                  allocate(grown(n + 1))
                  if (n > 0) grown(1:n) = unresolved
                  grown(n + 1) = entry
                  call move_alloc(grown, unresolved)
               end if
            class default
               _FAIL('GraphBuilder: unresolved_import_node_ids - import port NodeId is not a StateItemNode')
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end function graphbuilder_unresolved_import_node_ids

   ! Shared by graphbuilder_unresolved_import_node_ids above (both
   ! case (a) and case (b)): does some MatchConnection declared on
   ! `owner`, whose destination resolves (resolve_qualified_component)
   ! to `target_meta` exactly, match `node`'s own short name AND find a
   ! real source export for it? Same per-item positive check
   ! check_match_connection_unsatisfied's own check_one already makes
   ! (callback-interface branch via resolve_callback_import, ordinary
   ! branch via find_export_node) - reused here as a POSITIVE match
   ! collector (returns as soon as one resolving connection is found)
   ! instead of a negative "report every miss" one. Read-only.
   function connection_resolves_import(owner, target_meta, node, rc) result(resolved)
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(OuterMetaComponent), target, intent(in) :: target_meta
      class(StateItemNode), target, intent(in) :: node
      integer, optional, intent(out) :: rc
      logical :: resolved

      integer :: status
      type(ComponentSpec), pointer :: owner_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(ConnectionPt) :: src_pt, dst_pt
      type(OuterMetaComponent), pointer :: dst_meta, src_meta
      type(CallbackInterfaceId) :: callback_interface_id
      type(QualifiedExportEntry), allocatable :: callback_matches(:)
      logical :: has_export

      resolved = .false.
      owner_spec => owner%get_component_spec()

      associate (e => owner_spec%connections%ftn_end())
         iter = owner_spec%connections%ftn_begin()
         do while (iter /= e)
            call iter%next()
            c => iter%of()
            select type (c)
            class is (MatchConnection)
               src_pt = c%get_source()
               dst_pt = c%get_destination()

               dst_meta => resolve_qualified_component(owner, dst_pt%component_name, _RC)
               if (.not. associated(dst_meta, target_meta)) cycle
               if (.not. dst_pt%v_pt%matches(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, node%get_short_name()))) cycle

               callback_interface_id = node%get_callback_interface_id()
               if (callback_interface_id%is_valid()) then
                  callback_matches = resolve_callback_import(owner, src_pt, callback_interface_id, rc=status)
                  _VERIFY(status)
                  if (size(callback_matches) > 0) then
                     resolved = .true.
                     exit
                  end if
                  cycle
               end if

               src_meta => resolve_qualified_component(owner, src_pt%component_name, _RC)
               has_export = associated(find_export_node(src_meta, node%get_short_name()))
               if (has_export) then
                  resolved = .true.
                  exit
               end if
            class default
               ! openspec/changes/hierarchy-wide-component-graph, tasks.md
               ! 6.8 (regression fix, second gap - distinct from the
               ! item-type exclusion above): a SimpleConnection/
               ! ReexportConnection declared on `owner`, whose
               ! destination names `target_meta` directly by its own
               ! EXACT (non-regex) short name, is StateRegistry's own
               ! real resolver for this import - never visible to
               ! graph-native resolution at all (GraphBuilder.F90's own
               ! module header: "Explicitly NOT in scope here: Simple
               ! Connection/ReexportConnection"), but genuinely
               ! resolved nonetheless (legacy's own is_active() gate
               ! does not discriminate by connection class either -
               ! SimpleConnection%activate(), StateRegistry/
               ! SimpleConnection.F90, calls spec%activate() exactly
               ! like MatchConnection's own analog). Treating "named by
               ! this connection, exactly" as sufficient here (no
               ! find_export_node call - this import's own real
               ! producer is resolved entirely within StateRegistry,
               ! which this procedure has no business re-verifying)
               ! prevents propagation from needlessly bubbling an
               ! import that already has a real, declared resolver one
               ! level up.
               dst_pt = c%get_destination()
               dst_meta => resolve_qualified_component(owner, dst_pt%component_name, _RC)
               if (.not. associated(dst_meta, target_meta)) cycle
               if (dst_pt%get_esmf_name() /= node%get_short_name()) cycle
               resolved = .true.
               exit
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end function connection_resolves_import

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.1/6.2,
   ! design.md D3: see this function's own interface-block comment
   ! (GraphBuilder.F90) for invocation-point reasoning. For each direct
   ! child C of `this`, obtains C's own still-unresolved imports
   ! (graphbuilder_unresolved_import_node_ids, above, invoked on C's
   ! behalf through the framework-internal get_child_component_graph
   ! carve-out - REQ-GB-002) and mirrors each one into `this`'s own
   ! import-port table under C's own already-real NodeId. No new node,
   ! no proxy, no composite wrapper (design.md D3) - the mirrored
   ! entry's own NodeId is additionally reachable through `this`'s own
   ! import ports, in addition to C's own, so for_each_matching_import
   ! (unmodified) finds it exactly as it would any import `this`
   ! advertised directly.
   !
   ! Idempotency (found during real-lifecycle testing, both NAG and
   ! gfortran - a hang under gfortran, repeat-logged field-dictionary
   ! warnings growing without bound under both): ESMF's own
   ! Provide/Accept/Realize transfer negotiation can invoke
   ! GENERIC_INIT_ADVERTISE (and hence this hook) more than once per
   ! component within a single Initialize sequence - the same
   ! "idempotency" concern graphbuilder_resolve_connections' own header
   ! comment already documents for the connect()-time analog. Unlike
   ! that procedure (guarded by frozen-graph state, not yet reached at
   ! ADVERTISE time), this hook has no such state to check - guarded
   ! instead by skipping any NodeId already present as a VALUE
   ! somewhere in `this`'s own import-port table (already_mirrored,
   ! below): a repeat call re-derives the exact same unresolved-entry
   ! set from C's own (unchanged) graph state, so "already a value
   ! here" is both necessary and sufficient to detect "already mirrored
   ! by an earlier call," with no additional bookkeeping needed.
   module subroutine graphbuilder_run_propagate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      integer :: i, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta
      type(ComponentGraph), pointer :: this_graph
      type(UnresolvedImportEntry), allocatable :: unresolved(:)
      type(PortId) :: port_id
      integer :: j

      this_graph => this%get_component_graph()
      num_children = this%get_num_children()

      do i = 1, num_children
         child_name = this%get_child_name(i, status)
         call report_if_failed(this, 'run_propagate_hook:get_child_name', status)
         if (status /= 0) cycle

         child_meta => this%get_child_outer_meta(child_name, status)
         call report_if_failed(this, 'run_propagate_hook:get_child_outer_meta', status)
         if (status /= 0) cycle

         unresolved = graphbuilder_unresolved_import_node_ids(this, child_meta, status)
         call report_if_failed(this, 'run_propagate_hook:unresolved_import_node_ids', status)
         if (status /= 0) cycle

         do j = 1, size(unresolved)
            if (already_mirrored(this_graph, unresolved(j)%node_id)) cycle

            port_id = this_graph%next_port_id(status)
            call report_if_failed(this, 'run_propagate_hook:next_port_id', status)
            if (status /= 0) cycle
            call this_graph%add_import_port(port_id, unresolved(j)%node_id, status)
            call report_if_failed(this, 'run_propagate_hook:add_import_port', status)
         end do
      end do
   end subroutine graphbuilder_run_propagate_hook

   ! Idempotency helper for graphbuilder_run_propagate_hook above: does
   ! graph's own import-port table already contain target_id as a
   ! VALUE (not a key - PortId keys are always freshly minted, so a key
   ! check would never match)? A plain linear scan - this table is
   ! bounded by one component's own import count, not hierarchy-wide.
   function already_mirrored(graph, target_id) result(found)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: target_id
      logical :: found

      type(PortIdNodeIdMap), target :: import_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(NodeId) :: candidate

      found = .false.
      import_ports = graph%get_import_ports()
      associate (e => import_ports%ftn_end())
         iter = import_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate = iter%second()
            if (candidate == target_id) then
               found = .true.
               return
            end if
         end do
      end associate
   end function already_mirrored

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.8: see
   ! this subroutine's own interface-block comment (GraphBuilder.F90)
   ! for the full rationale. For each direct child C of `this`,
   ! propagates every one of C's own export-port entries into `this`'s
   ! own export-port table, under C's own already-real NodeId, with an
   ! effective qualified name computed by qualify_exported_name below.
   ! Unconditional - unlike import propagation, there is no "already
   ! resolved" gate (legacy's own propagate_exports_subregistry has
   ! none either; an export's own usefulness is determined entirely by
   ! whether some descendant's import ultimately matches it, which is
   ! not knowable, and not needed, at the point export propagation
   ! itself runs).
   module subroutine graphbuilder_run_export_propagate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      integer :: i, num_children
      character(:), allocatable :: child_name
      type(OuterMetaComponent), pointer :: child_meta
      type(ComponentGraph), pointer :: this_graph
      type(ComponentGraph), pointer :: child_graph
      type(PortIdNodeIdMap), target :: child_export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(PortId) :: child_port_id
      type(NodeId) :: export_node_id
      character(:), allocatable :: qualified_name
      type(PortId) :: new_port_id

      this_graph => this%get_component_graph()
      num_children = this%get_num_children()

      do i = 1, num_children
         child_name = this%get_child_name(i, status)
         call report_if_failed(this, 'run_export_propagate_hook:get_child_name', status)
         if (status /= 0) cycle

         child_meta => this%get_child_outer_meta(child_name, status)
         call report_if_failed(this, 'run_export_propagate_hook:get_child_outer_meta', status)
         if (status /= 0) cycle

         child_graph => child_meta%get_component_graph()
         child_export_ports = child_graph%get_export_ports()

         associate (e => child_export_ports%ftn_end())
            iter = child_export_ports%ftn_begin()
            do while (iter /= e)
               call iter%next()
               child_port_id = iter%first()
               export_node_id = iter%second()

               qualified_name = qualify_exported_name(child_graph, child_port_id, child_name)
               if (.not. allocated(qualified_name)) cycle

               if (already_exported(this_graph, export_node_id, qualified_name)) cycle

               new_port_id = this_graph%next_port_id(status)
               call report_if_failed(this, 'run_export_propagate_hook:next_port_id', status)
               if (status /= 0) cycle
               call this_graph%add_export_port(new_port_id, export_node_id, status, &
                    qualified_name=qualified_name)
               call report_if_failed(this, 'run_export_propagate_hook:add_export_port', status)
            end do
         end associate
      end do
   end subroutine graphbuilder_run_export_propagate_hook

   ! Shared helper for graphbuilder_run_export_propagate_hook above:
   ! computes the qualified name a child's own export-port entry
   ! (child_port_id, in child_graph) should carry once propagated into
   ! the PARENT's own export-port table, given the child's own name
   ! (child_name). Per the user's own confirmation of legacy's exact
   ! rule (propagate_exports_virtual_pt, StateRegistry_Propagation_
   ! smod.F90: "comp_name == ''" -> qualify with comp_name=subregistry_name;
   ! already non-empty -> passed through UNCHANGED, never re-qualified
   ! at any further ancestor level):
   !   - if child_graph's own get_export_port_name(child_port_id)
   !     already carries an override (this entry was ITSELF propagated
   !     up from a grandchild, at an earlier level) - the override is
   !     already fully qualified (e.g. "A/E") - returned UNCHANGED, no
   !     further qualification added.
   !   - otherwise (child_port_id is the child's OWN direct export,
   !     with no override yet) - qualified as child_name // '/' //
   !     the underlying node's own real short_name.
   function qualify_exported_name(child_graph, child_port_id, child_name) result(qualified_name)
      type(ComponentGraph), target, intent(in) :: child_graph
      type(PortId), intent(in) :: child_port_id
      character(*), intent(in) :: child_name
      character(:), allocatable :: qualified_name

      character(:), allocatable :: own_name

      own_name = child_graph%get_export_port_name(child_port_id)
      if (.not. allocated(own_name)) return

      ! own_name already equals the node's own real short_name (no
      ! override present) iff child_port_id has no recorded override -
      ! distinguished here not by string content (a short_name could
      ! coincidentally contain '/') but by re-deriving the SAME
      ! "override present?" check get_export_port_name itself makes
      ! internally. Since that check is not separately exposed, the
      ! equivalent, correct distinction is: an override-carrying name
      ! was placed there by THIS SAME propagation step at an earlier
      ! ancestor level, which only ever happens for an entry this
      ! child's own export-port table received via ITS OWN
      ! run_export_propagate_hook call (i.e. the child is itself an
      ! intermediate level with a further descendant) - own_name
      ! containing '/' is the correct, sufficient signal in that case,
      ! since a grandchild-propagated name is always
      ! "descendant_name/short_name" by construction, and MAPL child
      ! names are themselves plain identifiers (no '/' of their own -
      ! REQ-HIER, component-name uniqueness/validity), so a directly-
      ! advertised export's own real short_name containing '/' would
      ! itself be a pre-existing, unrelated naming oddity, not a false
      ! positive this logic introduces.
      if (index(own_name, '/') > 0) then
         qualified_name = own_name
      else
         qualified_name = child_name // '/' // own_name
      end if
   end function qualify_exported_name

   ! Idempotency helper for graphbuilder_run_export_propagate_hook
   ! above, symmetric to already_mirrored: does `graph`'s own
   ! export-port table already contain an entry whose VALUE is
   ! target_id AND whose own effective name (get_export_port_name)
   ! equals target_name? Checking both (not node_id alone, as
   ! already_mirrored does for imports) is necessary here because a
   ! single export NodeId can legitimately appear under more than one
   ! qualified name at different ancestor levels during the SAME
   ! propagation pass in a diamond-shaped hierarchy - task 6.8 does not
   ! need to guard against that (unlike task 6.1's own single-name-per-
   ! import invariant), only against re-adding the IDENTICAL
   ! (node_id, name) pair on a repeat ADVERTISE call (the same ESMF
   ! Provide/Accept/Realize re-entrancy concern already_mirrored's own
   ! header comment documents).
   function already_exported(graph, target_id, target_name) result(found)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: target_id
      character(*), intent(in) :: target_name
      logical :: found

      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(PortId) :: candidate_port_id
      type(NodeId) :: candidate_id
      character(:), allocatable :: candidate_name

      found = .false.
      export_ports = graph%get_export_ports()
      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate_port_id = iter%first()
            candidate_id = iter%second()
            if (.not. (candidate_id == target_id)) cycle
            candidate_name = graph%get_export_port_name(candidate_port_id)
            if (.not. allocated(candidate_name)) cycle
            if (candidate_name == target_name) then
               found = .true.
               return
            end if
         end do
      end associate
   end function already_exported

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
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.8:
   ! matches against each export port's own effective NAME
   ! (ComponentGraph%get_export_port_name - the export-propagation
   ! override when one exists, else the node's own real short_name),
   ! not the node's own short_name directly - a propagated export's
   ! own real StateItemNode%short_name stays unqualified (it is a
   ! different component's own identity, never rewritten), while this
   ! graph's own export-port TABLE entry for it carries the ancestor-
   ! qualified name (`child_name/short_name`) ordinary name matching
   ! must compare against, exactly mirroring legacy's own
   ! propagate_exports_virtual_pt comp_name-qualification (StateRegistry_
   ! Propagation_smod.F90).
   function find_export_node(src_meta, short_name) result(export_node)
      class(OuterMetaComponent), target, intent(inout) :: src_meta
      character(*), intent(in) :: short_name
      class(StateItemNode), pointer :: export_node

      type(ComponentGraph), pointer :: src_graph
      type(PortIdNodeIdMap), target :: export_ports
      type(PortIdNodeIdMapIterator) :: iter
      type(PortId) :: candidate_port_id
      type(NodeId) :: candidate_id
      class(GraphNode), pointer :: candidate_node
      character(:), allocatable :: candidate_name

      export_node => null()

      src_graph => src_meta%get_component_graph()
      export_ports = src_graph%get_export_ports()

      associate (e => export_ports%ftn_end())
         iter = export_ports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            candidate_port_id = iter%first()
            candidate_id = iter%second()
            candidate_node => src_graph%get_node(candidate_id)
            if (.not. associated(candidate_node)) cycle
            select type (candidate_node)
            class is (StateItemNode)
               candidate_name = src_graph%get_export_port_name(candidate_port_id)
               if (.not. allocated(candidate_name)) cycle
               if (candidate_name == short_name) then
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

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.8
   ! (same correction task 6.3 already made for the import side, found
   ! again here on the export side while fixing history_1's own real
   ! regression): export_node is already the exact match find_export_node
   ! found by walking src_meta's own export-port table - its own real
   ! NodeId (get_node_id(), BaseGraphNode's own accessor) is used
   ! directly instead of re-deriving the same id via a SEPARATE
   ! resolve_local_node_id resource-index lookup keyed by `short_name`.
   ! That old lookup is wrong for a PROPAGATED export (task 6.8): the
   ! resource index on src_meta (comp_name's own resolved component)
   ! only ever has an entry under the item's own real, UNQUALIFIED
   ! short_name (advertise_one's own add_resource_index call) - never
   ! under the qualified name (`child_name/short_name`)
   ! find_export_node now matches against (find_export_node's own
   ! header comment, this same file). Using export_node's own id
   ! directly is strictly equivalent to the old lookup for every
   ! directly-advertised export too (both resolve to the same NodeId),
   ! so this is behavior-preserving for every pre-existing caller, not
   ! a new resolution path.
   logical module function find_export_node_id(this, comp_name, short_name, export_node_id, export_node, rc) result(found)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      character(*), intent(in) :: short_name
      type(NodeId), intent(out) :: export_node_id
      class(StateItemNode), pointer, intent(out) :: export_node
      integer, optional, intent(out) :: rc

      integer :: status
      type(OuterMetaComponent), pointer :: src_meta

      src_meta => resolve_qualified_component(this, comp_name, _RC)
      export_node => find_export_node(src_meta, short_name)
      found = associated(export_node)

      if (.not. found) then
         _RETURN(_SUCCESS)
      end if

      export_node_id = export_node%get_node_id()

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
