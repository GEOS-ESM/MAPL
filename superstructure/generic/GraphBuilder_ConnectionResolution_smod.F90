#include "MAPL.h"

! Task 3: Ordinary connection resolution
! (openspec/changes/graphbuilder-submodule-split: procedure bodies moved
! here verbatim from GraphBuilder.F90, mechanical subroutine->module
! subroutine / function->module function change only - see that
! change's design.md D1 for the full section-to-file mapping.)

submodule (mapl_GraphBuilder_mod) GraphBuilder_ConnectionResolution_smod
   use esmf, only: ESMF_State, ESMF_StateGet, ESMF_STATEITEM_NOTFOUND, operator(==)
   use mapl_CharacteristicId_mod, only: CharacteristicId, TYPE_KIND_CHARACTERISTIC_ID, UNITS_CHARACTERISTIC_ID, VERTICAL_GRID_CHARACTERISTIC_ID
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator, operator(/=)
   use mapl_ExtensionResolution_mod, only: find_mismatched_characteristics, find_or_build_extension_chain, materialize_extensions_enabled
   use mapl_FieldNamedAlias_mod, only: MAPL_NamedAlias
   use mapl_GraphMode_mod, only: graph_mode, GRAPH_MODE_REGISTRY
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_GraphResolutionEntry_mod, only: GraphResolutionEntry
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_VERTICALGRID, operator(==)
   use mapl_TypeKindCharacteristicAdapter_mod, only: TypeKindCharacteristicAdapter
   use mapl_UnitsCharacteristic_mod, only: UnitsCharacteristic
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_VerticalGridCharacteristic_mod, only: VerticalGridCharacteristic
   use mapl_ErrorHandling_mod
   implicit none(type, external)

contains

   ! Read-only counterpart of resolve_match_connection() below: same
   ! destination-import filtering and same-name export existence check,
   ! but never calls get_or_make_local_node_id() or add_dependency() -
   ! no proxy node or dependency edge is created here.
   module subroutine check_match_connection_unsatisfied(this, conn, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(ComponentSpec), pointer :: dst_spec, src_spec
      type(OuterMetaComponent), pointer :: dst_meta, src_meta

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      ! openspec/changes/graph-builder-resolution-index: resolved via
      ! resolve_qualified_component (reaching any descendant, not only
      ! "<self>" or a direct child) rather than component_spec_for's own
      ! single-level dispatch - see
      ! GraphBuilder_ConnectionResolution_smod.F90's module header for
      ! this capability's scope.
      dst_meta => resolve_qualified_component(this, dst_pt%component_name, _RC)
      dst_spec => dst_meta%get_component_spec()
      src_meta => resolve_qualified_component(this, src_pt%component_name, _RC)
      src_spec => src_meta%get_component_spec()

      call for_each_matching_import(dst_spec, dst_pt, check_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine check_one(var_spec, rc)
         type(VariableSpec), intent(in) :: var_spec
         integer, optional, intent(out) :: rc

         logical :: has_export
         type(QualifiedExportEntry), allocatable :: callback_matches(:)

         ! openspec/changes/callback-wiring, design.md Decision 0: a
         ! destination declaring an expected callback interface is
         ! resolved against the flattened namespace instead of exact-
         ! name matching - read-only here (resolve_callback_import never
         ! mutates the graph), matching this procedure's own activate-
         ! time contract.
         if (var_spec%callback_interface_id%is_valid()) then
            callback_matches = resolve_callback_import(this, src_pt, var_spec, rc=rc)
            if (size(callback_matches) == 0) then
               call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name))
            end if
            _RETURN(_SUCCESS)
         end if

         has_export = associated(find_export_var_spec(src_spec, var_spec%short_name))
         if (.not. has_export) then
            call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name))
         end if

         _RETURN(_SUCCESS)
      end subroutine check_one

   end subroutine check_match_connection_unsatisfied

   ! Mirrors MatchConnection%activate()'s own two-step structure
   ! (superstructure/generic/connection/MatchConnection.F90) - filter the
   ! destination's imports against the declared destination pattern, then
   ! for each match require an EXACT-name export on the source side -
   ! but recomputes it directly against ComponentSpec%var_specs
   ! (design.md Decisions: "reuses MatchConnection's semantics, not its
   ! implementation") rather than going through StateRegistry's
   ! subregistry/family machinery. VirtualConnectionPt%matches() and the
   ! ConnectionPt/VirtualConnectionPt read-only accessors are ordinary
   ! public query methods on the Connection's own declared data, not part
   ! of that execution machinery, and are used here as-is.
   module subroutine resolve_match_connection(this, conn, unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(ComponentSpec), pointer :: dst_spec
      type(OuterMetaComponent), pointer :: dst_meta
      type(ComponentGraph), pointer :: this_graph
      type(DependencyNetworkId) :: net_id

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      ! openspec/changes/graph-builder-resolution-index: see
      ! check_match_connection_unsatisfied's own identical replacement
      ! above - resolve_qualified_component in place of
      ! component_spec_for's own single-level dispatch.
      dst_meta => resolve_qualified_component(this, dst_pt%component_name, _RC)
      dst_spec => dst_meta%get_component_spec()

      this_graph => this%get_component_graph()
      net_id = this_graph%get_default_network_id()

      call for_each_matching_import(dst_spec, dst_pt, resolve_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine resolve_one(var_spec, rc)
         type(VariableSpec), intent(in) :: var_spec
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
         type(VariableSpec), pointer :: export_var_spec
         type(CharacteristicMap), target :: export_characteristics, import_characteristics
         type(CharacteristicId), allocatable :: mismatched(:)
         character(:), allocatable :: unsupported_characteristic
         character(:), allocatable :: materialization_failure
         type(StringVector) :: callback_rejected
         character(:), pointer :: rejected_item
         integer :: j

         ! openspec/changes/ordinary-inout-direct-alias: a destination
         ! declaring BOTH an expected callback interface and ordinary
         ! inout borrower intent is a nonsensical combination - two
         ! separate, mutually exclusive resolution paths exist below
         ! (callback-interface branch, then inout-borrower branch), and
         ! resolving via whichever branch happens to run first would
         ! silently ignore the other declaration rather than honoring it.
         ! Rejected explicitly, checked before either branch, so neither
         ! one ever silently wins.
         if (var_spec%callback_interface_id%is_valid() .and. var_spec%is_inout_borrower) then
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
                 'declares both an expected callback interface and inout borrower intent (mutually exclusive)'))
            _RETURN(_SUCCESS)
         end if

         ! openspec/changes/callback-wiring, design.md Decision 0: a
         ! destination declaring an expected callback interface is
         ! resolved against the flattened namespace, materialized into
         ! its own collection, and wired to the import via an ordinary
         ! add_dependency edge (resolve_callback_destination) - instead
         ! of the exact-name/extension-chain logic below, which never
         ! runs for this var_spec.
          if (var_spec%callback_interface_id%is_valid()) then
             call resolve_callback_destination(this, src_pt, dst_pt, var_spec, callback_rejected, _RC)
             do j = 1, callback_rejected%size()
                rejected_item => callback_rejected%of(j)
                call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
                     rejected_item))
             end do
             _RETURN(_SUCCESS)
          end if

          ! openspec/changes/ordinary-inout-direct-alias, design.md
          ! Decisions 1/4: a destination declared as an ordinary inout
          ! borrower is resolved by resolve_inout_destination instead of
          ! the exact-name/extension-chain logic below, which never runs
          ! for this var_spec - mirrors the callback-interface branch
          ! immediately above exactly.
          if (var_spec%is_inout_borrower) then
             call resolve_inout_destination(this, src_pt, dst_pt, var_spec, unsupported, _RC)
             _RETURN(_SUCCESS)
          end if

          import_node_id = get_or_make_local_node_id(this, dst_pt%component_name, &
              ESMF_STATEINTENT_IMPORT, var_spec%short_name, _RC)

         has_export = find_export_node_id(this, src_pt%component_name, var_spec%short_name, &
              export_node_id, export_var_spec, _RC)

         if (.not. has_export) then
            ! REQ scenario "Import with no matching export is left
            ! unresolved" - reported, not silently dropped.
            call unresolved%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name))
            _RETURN(_SUCCESS)
         end if

         export_characteristics = build_characteristics(export_var_spec, _RC)
         import_characteristics = build_characteristics(var_spec, _RC)
         mismatched = find_mismatched_characteristics(export_characteristics, import_characteristics)

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
               ! the whole point). Observed (not just statically traced,
               ! tasks.md 3.1), running Test_GraphModeDemonstration.pf's
               ! own units_mismatch_run_phase fixture under a temporary
               ! diagnostic print removed before this comment was
               ! written: try_alias_export_field succeeds (found=.true.)
               ! on every call for this fixture, including the FIRST
               ! GENERIC_INIT_ACCEPT_TRANSFER pass - confirming design.md
               ! D2's static trace; the FieldCreate fallback below is
               ! never actually exercised for this fixture.
               call try_alias_export_field(this, src_pt%component_name, export_var_spec, &
                    this_graph, export_node_id, alias_found, _RC)
               if (.not. alias_found) then
                  call try_materialize_field_extension(this, src_pt%component_name, export_var_spec, var_spec, &
                       this_graph, export_node_id, materialization_failure, name=export_var_spec%short_name, _RC)
                  if (materialization_failure /= '') then
                     ! Same distinguishable-failure reporting the mismatch
                     ! branch below already uses - the edge above still
                     ! exists (wiring succeeded; only payload materialization
                     ! did not).
                     call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
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
              export_characteristics, import_characteristics, mismatched, &
              final_node_id, intermediate_node_ids, unsupported_characteristic, _RC)

         if (unsupported_characteristic /= '') then
            ! spec "Unregistered characteristic fails loudly" -
            ! distinguishable from "import has no matching export."
            call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
                 unsupported_characteristic))
            _RETURN(_SUCCESS)
         end if

         ! openspec/changes/graph-child-state-population, found during
         ! implementation (this change's own YAML-configured
         ! demonstration scenario): the mismatch branch's own
         ! export_node_id (the chain's own INPUT) was never materialized
         ! by anything - only the exact-match branch above (line ~849)
         ! and the chain's own FINAL node (below) ever call
         ! try_materialize_field_extension. Harmless as long as nothing
         ! ever executes the chain's own compute() for real (no fixture
         ! did, before this change's own run-phase pull, design.md D7);
         ! UnitsConverterTransform%compute()'s own
         ! `input_field = input_item%get_field(_RC)` call fails outright
         ! on an unallocated input otherwise. Fixed by materializing
         ! export_node_id here too, using the EXPORT's own typekind/units/
         ! geom/vgrid - NOT the import's (passing export_var_spec for
         ! BOTH the export_var_spec and import_var_spec arguments is what
         ! makes try_materialize_field_extension's own "always use the
         ! second VariableSpec argument's typekind/units" contract
         ! resolve to the export's own values here, correct for the
         ! chain's own input node, as opposed to the chain's final node
         ! below, which correctly keeps using the import's own values).
         ! Gated the same way the exact-match branch's own materialization
         ! already is (graph_mode() /= GRAPH_MODE_REGISTRY) - a no-op in
         ! GRAPH_MODE_REGISTRY, matching every other new code path this
         ! change adds.
         if (graph_mode() /= GRAPH_MODE_REGISTRY) then
            ! openspec/changes/phase-11a-export-field-aliasing: same
            ! guard, same skip-when-found treatment as the exact-match
            ! branch above (D4) - no divergence between the two call
            ! sites. export_var_spec is passed for both the "which
            ! component/item to alias" argument and (unchanged, in the
            ! fallback below) the typekind/units source - the chain's
            ! own input node correctly uses the export's own values,
            ! same as before this change.
            call try_alias_export_field(this, src_pt%component_name, export_var_spec, &
                 this_graph, export_node_id, alias_found, _RC)
            if (.not. alias_found) then
               call try_materialize_field_extension(this, src_pt%component_name, export_var_spec, export_var_spec, &
                    this_graph, export_node_id, materialization_failure, name=export_var_spec%short_name, _RC)
               if (materialization_failure /= '') then
                  call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
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
            call materialize_extension_chain(this, src_pt%component_name, export_var_spec, var_spec, &
                 mismatched, intermediate_node_ids, this_graph, materialization_failure, _RC)
            if (materialization_failure /= '') then
               ! Distinguishable from both "no matching export" and
               ! "unregistered characteristic fails loudly" above (spec
               ! "A non-field item class fails explicitly").
               call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
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
   ! this same "which of dst_spec's imports does dst_pt's declared
   ! pattern match" do/if/if scan (module header - "same destination-
   ! import filtering"). Owns the loop and both guard clauses; each
   ! caller supplies only the per-match step, as an internal procedure so
   ! it can host-associate whatever extra context it needs (this,
   ! src_pt, unresolved, ...) - same style as OuterMetaComponent's own
   ! apply_to_children_custom()/I_child_op.
   module subroutine for_each_matching_import(dst_spec, dst_pt, op, rc)
      type(ComponentSpec), intent(in) :: dst_spec
      type(ConnectionPt), intent(in) :: dst_pt
      procedure(I_match_op) :: op
      integer, optional, intent(out) :: rc

      integer :: status
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      associate (e => dst_spec%var_specs%ftn_end())
         iter = dst_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_IMPORT)) cycle
            if (.not. dst_pt%v_pt%matches(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, var_spec%short_name))) cycle
            call op(var_spec, _RC)
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

   ! Builds the CharacteristicMap for one VariableSpec directly from its
   ! own declared fields - deliberately not via
   ! VariableSpec%make_StateitemSpec/make_aspects (design.md Decisions:
   ! "Characteristic is the graph's own name for what legacy calls an
   ! Aspect, and is a deliberately independent design, not a reskin").
   ! A kind is included only when the underlying field is actually
   ! populated, mirroring "aspect present" gating.
    module function build_characteristics(var_spec, rc) result(characteristics)
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc
      type(CharacteristicMap) :: characteristics

      integer :: status
      character(20) :: grid_id_buffer

      if (allocated(var_spec%units)) then
         call characteristics%insert(UNITS_CHARACTERISTIC_ID, UnitsCharacteristic(var_spec%units))
      end if
      ! openspec/changes/phase-10c-typekind-characteristic, design.md D3:
      ! unlike units, VariableSpec%typekind is a non-allocatable field
      ! with a default (ESMF_TYPEKIND_R4) - every VariableSpec always
      ! has SOME typekind, so there is no "not declared" state to gate
      ! on the way `allocated(var_spec%units)` gates the branch above.
      ! Inserted unconditionally; see design.md D3 for the regression
      ! argument (an exact-match connection's two sides already agree by
      ! construction in every existing fixture, so this adds a real but
      ! always-no-op comparison for them, changing no existing resolved
      ! result).
      call characteristics%insert(TYPE_KIND_CHARACTERISTIC_ID, TypeKindCharacteristicAdapter(var_spec%typekind))
      if (allocated(var_spec%vertical_grid)) then
         ! VerticalGrid%get_id() returns an integer identity token;
         ! VerticalGridCharacteristic compares an opaque string
         ! signature (Characteristic.F90 get_signature contract), so it
         ! is rendered to text here rather than changing
         ! VerticalGridCharacteristic's own constructor to know about
         ! integer ids specifically. get_supported_physical_dimensions()
         ! is a pure accessor (no StateRegistry involvement, unlike
         ! get_coordinate_field()) - also supplied here so a mismatched
         ! identity gets REQ-GEO-007a's dimension-overlap classification
         ! (openspec/changes/vertical-grid-graph-state-item) rather than
         ! an undifferentiated failure.
         write(grid_id_buffer, '(I0)') var_spec%vertical_grid%get_id()
         call characteristics%insert(VERTICAL_GRID_CHARACTERISTIC_ID, &
              VerticalGridCharacteristic(grid_id=trim(grid_id_buffer), &
                   dimensions=var_spec%vertical_grid%get_supported_physical_dimensions()))
      end if
      ! openspec/changes/vertical-grid-graph-state-item: a composite
      ! VariableSpec tagged state_item_variant == MAPL_STATEITEM_VERTICALGRID
      ! (REQ-GEO-009) has no legacy VerticalGrid object to pull an
      ! identity token from - only its own declared member names
      ! (REQ-GEO-004a: member name = physical dimension, so
      ! get_member_names() already IS the dimension set). Uses the SAME
      ! VerticalGridCharacteristic/VERTICAL_GRID_CHARACTERISTIC_ID as the
      ! branch above (not a separate characteristic kind) - with no
      ! identity token supplied, needs_extension_for falls back to
      ! dimension-set comparison (VerticalGridCharacteristic.F90's own
      ! module header).
      if (allocated(var_spec%state_item_variant)) then
         if (var_spec%state_item_variant == MAPL_STATEITEM_VERTICALGRID) then
            call characteristics%insert(VERTICAL_GRID_CHARACTERISTIC_ID, &
                 VerticalGridCharacteristic(dimensions=var_spec%get_member_names()))
         end if
      end if

      _RETURN(_SUCCESS)
   end function build_characteristics

   logical module function find_export_node_id(this, comp_name, short_name, export_node_id, export_var_spec, rc) result(found)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      character(*), intent(in) :: short_name
      type(NodeId), intent(out) :: export_node_id
      type(VariableSpec), pointer, intent(out) :: export_var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: src_spec
      type(OuterMetaComponent), pointer :: src_meta

      export_var_spec => null()
      ! openspec/changes/graph-builder-resolution-index: see
      ! check_match_connection_unsatisfied's own identical replacement
      ! above - resolve_qualified_component in place of
      ! component_spec_for's own single-level dispatch. The export's
      ! real NodeId (get_or_make_local_node_id below) already reaches
      ! any descendant level via the same primitive, same change.
      src_meta => resolve_qualified_component(this, comp_name, _RC)
      src_spec => src_meta%get_component_spec()
      export_var_spec => find_export_var_spec(src_spec, short_name)
      found = associated(export_var_spec)
      if (.not. found) then
         _RETURN(_SUCCESS)
      end if

      export_node_id = get_or_make_local_node_id(this, comp_name, ESMF_STATEINTENT_EXPORT, short_name, _RC)

      _RETURN(_SUCCESS)
   end function find_export_node_id

   ! openspec/changes/ordinary-inout-direct-alias, design.md Decisions
   ! 1-4: resolves a destination item declared an ordinary inout borrower
   ! (16-inout-items.md REQ-INOUT-001, direct-alias case only). Reuses
   ! find_export_node_id/build_characteristics/
   ! find_mismatched_characteristics exactly as resolve_one's own
   ! ordinary (non-inout) path does for the REQ-EXT-003 no-op comparison
   ! - owner and borrower come to share one underlying payload through
   ! the same materialization path ordinary no-op connections already
   ! use, since the forward edge below is wired identically. On an exact
   ! match, also adds a return-network edge (borrower -> owner) in a
   ! fresh per-pairing network (Decision 2, revised - mirrors callback
   ! wiring's own per-binding create_network() precedent) so the pairing
   ! satisfies REQ-DEP-008a by construction (forward writes the
   ! borrower's NodeId, return writes the owner's NodeId - two different
   ! ids, never the same network). Every shape REQ-INOUT-002 reserves for
   ! a future design addendum (mismatched payload, no identifiable owner,
   ! chained/recursive borrowing) is rejected here and reported via
   ! `unsupported`, never delegated to extension-chain construction.
   ! Decision 3 (revised): this subroutine lands graph structure only -
   ! the runtime return-edge propagation trigger is explicitly deferred
   ! (no borrower-execution-completion hook exists yet to attach it to).
   module subroutine resolve_inout_destination(this, src_pt, dst_pt, var_spec, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      type(VariableSpec), intent(in) :: var_spec
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(DependencyNetworkId) :: net_id, return_net_id
      type(NodeId) :: import_node_id, owner_node_id
      logical :: has_owner
      type(VariableSpec), pointer :: owner_var_spec
      type(ComponentSpec), pointer :: owner_comp_spec
      type(CharacteristicMap), target :: owner_characteristics, borrower_characteristics
      type(CharacteristicId), allocatable :: mismatched(:)

      graph => this%get_component_graph()
      net_id = graph%get_default_network_id()

      import_node_id = get_or_make_local_node_id(this, dst_pt%component_name, &
           ESMF_STATEINTENT_IMPORT, var_spec%short_name, _RC)

      has_owner = find_export_node_id(this, src_pt%component_name, var_spec%short_name, &
           owner_node_id, owner_var_spec, _RC)

      if (.not. has_owner) then
         ! spec "A borrower declaration with no identifiable owner is
         ! rejected" - reported, never silently treated as an ordinary
         ! import.
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
              'inout borrower has no identifiable owner'))
         _RETURN(_SUCCESS)
      end if

      ! spec "A chained borrowing pairing is rejected": `owner_var_spec`
      ! (found above via find_export_node_id) is always the EXPORT-intent
      ! declaration on the source component - `is_inout_borrower` is only
      ! ever set on an IMPORT declaration (a borrower marks itself, not
      ! whoever re-exports its value), so checking
      ! `owner_var_spec%is_inout_borrower` directly would never detect
      ! chaining. Instead, check whether the source component itself
      ! separately declares a same-short-name IMPORT marked as an inout
      ! borrower - i.e. the value this destination would borrow is itself
      ! borrowed by its own owning component.
      owner_comp_spec => component_spec_for(this, src_pt%component_name, _RC)
      if (associated(find_inout_borrower_var_spec(owner_comp_spec, var_spec%short_name))) then
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
              'inout borrower owner is itself a declared inout borrower (chained borrowing)'))
         _RETURN(_SUCCESS)
      end if

      owner_characteristics = build_characteristics(owner_var_spec, _RC)
      borrower_characteristics = build_characteristics(var_spec, _RC)
      mismatched = find_mismatched_characteristics(owner_characteristics, borrower_characteristics)

      if (size(mismatched) /= 0) then
         ! spec "A mismatched pairing is rejected rather than silently
         ! chained" - REQ-INOUT-002 reserves the non-direct-alias case;
         ! this capability MUST NOT delegate to extension-chain
         ! construction the way ordinary (non-inout) resolution does.
         call unsupported%push_back(GraphResolutionEntry(dst_pt%component_name, var_spec%short_name, &
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

   module function find_export_var_spec(comp_spec, short_name) result(export_var_spec)
      type(ComponentSpec), target, intent(in) :: comp_spec
      character(*), intent(in) :: short_name
      type(VariableSpec), pointer :: export_var_spec

      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      export_var_spec => null()
      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT .and. var_spec%short_name == short_name) then
               export_var_spec => var_spec
               return
            end if
         end do
      end associate
   end function find_export_var_spec

   ! openspec/changes/ordinary-inout-direct-alias: finds a same-short-name
   ! IMPORT-intent VariableSpec on comp_spec that is itself declared an
   ! ordinary inout borrower (VariableSpec%is_inout_borrower) - used by
   ! resolve_inout_destination's chained-borrowing check (spec "A chained
   ! borrowing pairing is rejected"). Mirrors find_export_var_spec's own
   ! shape exactly, filtering IMPORT+is_inout_borrower instead of EXPORT.
   module function find_inout_borrower_var_spec(comp_spec, short_name) result(borrower_var_spec)
      type(ComponentSpec), target, intent(in) :: comp_spec
      character(*), intent(in) :: short_name
      type(VariableSpec), pointer :: borrower_var_spec

      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      borrower_var_spec => null()
      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (var_spec%state_intent == ESMF_STATEINTENT_IMPORT .and. var_spec%short_name == short_name &
                 .and. var_spec%is_inout_borrower) then
               borrower_var_spec => var_spec
               return
            end if
         end do
      end associate
   end function find_inout_borrower_var_spec

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
   module subroutine try_alias_export_field(this, export_comp_name, export_var_spec, &
        graph, node_id, found, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      type(VariableSpec), intent(in) :: export_var_spec
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
      type(GraphStateItem) :: payload

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
      call ESMF_StateGet(export_state, itemName=export_var_spec%short_name, itemType=item_kind, _RC)
      if (item_kind == ESMF_STATEITEM_NOTFOUND) then
         _RETURN(_SUCCESS)
      end if

      call ESMF_StateGet(export_state, itemName=export_var_spec%short_name, field=field, _RC)

      ! D5: MAPL_NamedAlias, not a hand-rolled ESMF_NamedAlias - carries
      ! MAPL's own per-alias-id metadata (standard_name/long_name/
      ! restart_mode), same as populate_one's own precedent.
      alias = MAPL_NamedAlias(field, name=export_var_spec%short_name, _RC)

      node => graph%get_node(node_id)
      _ASSERT(associated(node), 'GraphBuilder: try_alias_export_field - NodeId not found in graph')
      select type (node)
      class is (StateItemNode)
         call payload%set(alias, _RC)
         call node%set_payload(payload)
      class default
         _FAIL('GraphBuilder: try_alias_export_field - NodeId does not refer to a StateItemNode')
      end select

      found = .true.

      _RETURN(_SUCCESS)
   end subroutine try_alias_export_field

end submodule GraphBuilder_ConnectionResolution_smod
