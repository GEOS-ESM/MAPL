#include "MAPL.h"

!------------------------------------------------------------------------------
! GraphBuilder: the Phase 3b integration layer between the graph-neutral
! ComponentGraph/DependencyNetwork core and the rest of MAPL
! (spec/08-graph-builder.md REQ-GB-001/002/003, this change's proposal.md).
!
! Scope (roadmap sub-changes 3b + 3c, spec/20-implementation-roadmap.md
! sec 20.4.1): advertising (creating StateItemNodes for advertised
! import/export/internal items), ordinary (exact short-name match)
! connection resolution restricted to MatchConnection - the concrete
! Connection subtype behind OuterMetaComponent%connect_all's "magic
! connect" behavior (superstructure/generic/OuterMetaComponent/connect_all.F90)
! - public-port/child-proxy population, validate/freeze, and (3c,
! openspec/changes/extension-reuse) mismatch detection/extension-chain
! creation for a matched pair whose export does not exactly match its
! import, delegating to mapl_ExtensionResolution_mod
! (superstructure/generic/graph/ExtensionResolution.F90). Explicitly NOT
! in scope here: SimpleConnection/ReexportConnection, wildcard/callback
! resolution (Phase 4), compiled execution (Phase 5/Q9), and real
! (executing) extension providers for any characteristic other than
! `units` (3c's own scope boundary, extension-reuse change design.md) -
! see proposal.md "Explicitly out of scope".
!
! Design (design.md - Decisions): a stateless-per-call procedure set, not
! a persistent object - every procedure here takes the OuterMetaComponent
! it operates on as an explicit argument and reads/writes only that
! component's own ComponentGraph (obtained via the existing 3a accessor,
! get_component_graph) plus, for cross-boundary connection resolution,
! a named child's ComponentGraph/ComponentSpec reached via
! OuterMetaComponent's own get_child_component_graph()/
! get_child_component_spec() accessors (graphbuilder-code-quality-cleanup
! change) - a framework-internal carve-out (REQ-GB-002), not part of
! OuterMetaComponent's general public API (REQ-HIER-003/005), that these
! two accessors document at their own declaration site.
!
! Advertised-item and cross-graph-proxy identity lookup both reuse
! ComponentGraph's existing, already-specified "semantic resource index"
! (REQ-CG-001, add_resource_index()/get_resource_index()) rather than
! inventing a new lookup table: this is exactly the graph-neutral,
! public, name-keyed NodeId lookup REQ-CG-001 already provides, and using
! it here needs no ComponentGraph API change (design.md Non-Goals).
!
! Cross-component (parent/child) connections: a dependency edge can only
! be added to a network owned by the graph that owns both its endpoint
! NodeIds (ComponentGraph%add_dependency enforces this). A MatchConnection
! whose source or destination names a child therefore cannot be wired
! directly into that child's own network (nor would that be legal - a
! parent may not reach into a child's internal DependencyNetwork,
! REQ-HIER-005). Instead, for any endpoint that is not "<self>", this
! module creates (or reuses a cached) parent-local proxy StateItemNode
! standing in for that child's item (REQ-HIER-006), registers it in the
! PARENT's own graph, and records it via add_child_port_binding; the
! actual dependency edge is then always added to the calling component's
! own default network, between two of its own (real-or-proxy) NodeIds.
!
! Two-phase timing (matches the legacy Connection activate()/connect()
! split, not just its matching logic): the legacy imperative coupler
! does NOT form real wiring during GENERIC_INIT_ADVERTISE.
! Connection%activate() there only marks which imports/exports are
! "active", which existing MAPL machinery uses to decide (a) whether an
! unresolved import needs to bubble up to the parent
! (propagate_unsatisfied_imports) and (b) whether an export actually
! needs to be allocated at all (only allocated if some active import
! needs it) - see initialize_advertise.F90's process_connections()
! (calls activate()) versus initialize_accept_transfer.F90's
! process_connections() (calls connect(), the real wiring step).
! GraphBuilder mirrors this with two separate entry points instead of
! one:
!   - graphbuilder_check_unsatisfied_imports(): the activate()-time
!     analog - read-only, determines which ordinary-match imports would
!     be satisfied without creating any graph structure (no proxy nodes,
!     no dependency edges). Called from initialize_advertise.F90.
!   - graphbuilder_resolve_connections(): the connect()-time analog -
!     the real graph mutation (proxies + dependency edges). Called from
!     initialize_accept_transfer.F90, alongside the real connect() call,
!     followed by graphbuilder_freeze() there (freezing before real
!     wiring exists would be premature).
!------------------------------------------------------------------------------
module mapl_GraphBuilder_mod
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_ComponentSpec_mod, only: ComponentSpec
   use mapl_VariableSpec_mod, only: VariableSpec
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator
   use mapl_VariableSpecVector_mod, only: operator(/=)
   use mapl_Connection_mod, only: Connection
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_MatchConnection_mod, only: MatchConnection
   use mapl_ConnectionPt_mod, only: ConnectionPt
   use mapl_VirtualConnectionPt_mod, only: VirtualConnectionPt
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_NodeId_mod, only: NodeId
   use mapl_NodeLabel_mod, only: NodeLabel
   use mapl_NodeIdLabelMap_mod, only: NodeIdLabelMap
   use mapl_PortId_mod, only: PortId
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer
   use gFTL2_StringVector, only: StringVector
   use mapl_Characteristic_mod, only: CharacteristicMap
   use mapl_CharacteristicId_mod, only: CharacteristicId, UNITS_CHARACTERISTIC_ID, VERTICAL_GRID_CHARACTERISTIC_ID
   use mapl_UnitsCharacteristic_mod, only: UnitsCharacteristic
   use mapl_VerticalGridCharacteristic_mod, only: VerticalGridCharacteristic
   use mapl_ExtensionResolution_mod, only: find_mismatched_characteristics, find_or_build_extension_chain, &
        materialize_extensions_enabled
   use mapl_ExtensionMaterialization_mod, only: materialize_field_extension
   use mapl_StateItem_mod, only: MAPL_STATEITEM_FIELD, MAPL_STATEITEM_FIELDBUNDLE, &
        MAPL_STATEITEM_VECTOR, MAPL_STATEITEM_VECTORBRACKET, MAPL_STATEITEM_BRACKET, &
        MAPL_STATEITEM_STATE, MAPL_STATEITEM_SERVICE, MAPL_STATEITEM_EXPRESSION
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc, VERTICAL_STAGGER_CENTER, &
        VERTICAL_STAGGER_NONE, operator(==)
   use pflogger, only: Logger
   use esmf, only: ESMF_StateIntent_Flag, ESMF_STATEINTENT_IMPORT, &
        ESMF_STATEINTENT_EXPORT
   use esmf, only: ESMF_Geom, ESMF_StateItem_Flag
   use esmf, only: operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GraphBuilder
   public :: item_key
   public :: proxy_key

   ! Stateless-per-call by design (design.md Decisions): GraphBuilder
   ! carries no state of its own - every bound procedure still takes the
   ! OuterMetaComponent it operates on as an explicit argument, exactly
   ! as the free functions this type replaces did. All bindings are
   ! NOPASS so each procedure's signature is untouched by this
   ! type-bound wrapping; only the calling convention changes, e.g.
   ! `gb = GraphBuilder(); call gb%run_advertise_hook(this)`.
   type :: GraphBuilder
   contains
      procedure, nopass :: advertise => graphbuilder_advertise
      procedure, nopass :: check_unsatisfied_imports => graphbuilder_check_unsatisfied_imports
      procedure, nopass :: resolve_connections => graphbuilder_resolve_connections
      procedure, nopass :: freeze => graphbuilder_freeze
      procedure, nopass :: build_label_map => graphbuilder_build_label_map
      procedure, nopass :: run_advertise_hook => graphbuilder_run_advertise_hook
      procedure, nopass :: run_activate_hook => graphbuilder_run_activate_hook
      procedure, nopass :: run_connect_hook => graphbuilder_run_connect_hook
   end type GraphBuilder

   ! Matches StateRegistry_Hierarchy_smod's own SELF sentinel
   ! (superstructure/generic/registry/StateRegistry_Hierarchy_smod.F90):
   ! a ConnectionPt component_name of "<self>" always refers to the
   ! component the connection was declared on.
   character(*), parameter :: SELF_COMPONENT_NAME = '<self>'

   ! Per-match callback signature for for_each_matching_import() below -
   ! same shape/precedent as OuterMetaComponent's own I_child_op
   ! (superstructure/generic/OuterMetaComponent.F90), passed an internal
   ! (CONTAINS-nested) procedure by each caller so it can host-associate
   ! whatever extra context (this, src_pt, unresolved, ...) it needs
   ! without threading it through this interface.
   abstract interface
      subroutine I_match_op(var_spec, rc)
         import VariableSpec
         type(VariableSpec), intent(in) :: var_spec
         integer, optional, intent(out) :: rc
      end subroutine I_match_op
   end interface

contains

   ! ============================================================
   ! Task 2: Advertising
   ! ============================================================

   ! REQ-GB-003 "Advertising state items" / "Creating StateItemNodes":
   ! one StateItemNode per advertised import/export/internal item, plus
   ! (task 4.1) import/export items are recorded as public ports - MAPL's
   ! existing coupling semantics already treat every import/export as
   ! externally addressable (any sibling's connect_all can reach it by
   ! name) while internal items are component-private, so that existing
   ! distinction is reused here as "declared a public port" rather than
   ! inventing a new marker on VariableSpec/ComponentSpec.
   subroutine graphbuilder_advertise(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec

      comp_spec => this%get_component_spec()
      graph => this%get_component_graph()

      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            call advertise_one(graph, var_spec, _RC)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_advertise

   subroutine advertise_one(graph, var_spec, rc)
      type(ComponentGraph), intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: key
      type(NodeId), pointer :: existing
      type(NodeId) :: id
      type(StateItemNode) :: node
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(PortId) :: port_id

      key = item_key(var_spec%state_intent, var_spec%short_name)

      ! Idempotent re-advertisement (spec scenario "Re-advertising the
      ! same item does not duplicate its node"): if this identity is
      ! already indexed, this item already has a node - nothing to do.
      existing => graph%get_resource_index(key)
      if (associated(existing)) then
         _RETURN(_SUCCESS)
      end if

      id = graph%next_node_id(_RC)

      ! payload/revision are left default-initialized (no ESMF handle
      ! allocated, NodeRevision invalid): this item is advertised, not
      ! yet realized - REALIZE happens in a later init phase this slice
      ! does not touch (design.md Non-Goals).
      node = StateItemNode(id, payload, revision)
      call graph%register_node(node, _RC)
      call graph%add_resource_index(key, id, _RC)

      if (var_spec%state_intent == ESMF_STATEINTENT_IMPORT) then
         port_id = graph%next_port_id(_RC)
         call graph%add_import_port(port_id, id, _RC)
      else if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT) then
         port_id = graph%next_port_id(_RC)
         call graph%add_export_port(port_id, id, _RC)
      end if
      ! INTERNAL items get neither: they are not visible outside the
      ! component (task 4.1's "declared a public port" does not apply).

      _RETURN(_SUCCESS)
   end subroutine advertise_one

   ! Advertised-item identity key: state intent + short name, unique
   ! within one component (spec "Advertising creates graph state-item
   ! nodes" - "retrievable afterward by the item's identity"). Also used,
   ! unqualified by component, as the per-child lookup key when this
   ! component's own advertised items are read from a *different*
   ! component's ComponentGraph (see get_or_make_local_node_id below).
   function item_key(state_intent, short_name) result(key)
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      character(:), allocatable :: key

      character(:), allocatable :: intent_str

      if (state_intent == ESMF_STATEINTENT_IMPORT) then
         intent_str = 'IMPORT'
      else if (state_intent == ESMF_STATEINTENT_EXPORT) then
         intent_str = 'EXPORT'
      else
         intent_str = 'INTERNAL'
      end if
      key = intent_str // ':' // short_name
   end function item_key

   ! Resource-index key for a parent-local proxy node standing in for a
   ! named child's item (module header - "Cross-component (parent/child)
   ! connections"). Exposed (like item_key above) so callers with
   ! legitimate reason to identify a cached proxy - tests, or a later
   ! sub-change (3c) needing to find/reuse the same proxy - do not need
   ! to duplicate this module's private key format.
   function proxy_key(comp_name, state_intent, short_name) result(key)
      character(*), intent(in) :: comp_name
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      character(:), allocatable :: key

      key = 'PROXY:' // comp_name // ':' // item_key(state_intent, short_name)
   end function proxy_key

   ! ============================================================
   ! Task 3: Ordinary connection resolution
   ! ============================================================

   ! GENERIC_INIT_ADVERTISE-time analog of legacy Connection%activate()
   ! (module header - "Two-phase timing"): for each MatchConnection,
   ! determine which ordinary-match destination imports have a matching
   ! source export, WITHOUT creating any graph structure (no proxy
   ! nodes, no dependency edges - those are graphbuilder_resolve_connections'
   ! job, at connect() time). Read-only with respect to the graph;
   ! purely populates the unresolved-imports report, which is what
   ! legacy's own propagate_unsatisfied_imports() decision (bubble an
   ! unsatisfied import to the parent) is based on at this same phase.
   subroutine graphbuilder_check_unsatisfied_imports(this, unusable, unresolved_imports, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(StringVector), optional, intent(out) :: unresolved_imports
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(StringVector) :: unresolved

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
   subroutine for_each_matching_import(dst_spec, dst_pt, op, rc)
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

   ! Read-only counterpart of resolve_match_connection() below: same
   ! destination-import filtering and same-name export existence check,
   ! but never calls get_or_make_local_node_id() or add_dependency() -
   ! no proxy node or dependency edge is created here.
   subroutine check_match_connection_unsatisfied(this, conn, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
      type(StringVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(ComponentSpec), pointer :: dst_spec, src_spec

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      dst_spec => component_spec_for(this, dst_pt%component_name, _RC)
      src_spec => component_spec_for(this, src_pt%component_name, _RC)

      call for_each_matching_import(dst_spec, dst_pt, check_one, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine check_one(var_spec, rc)
         type(VariableSpec), intent(in) :: var_spec
         integer, optional, intent(out) :: rc

         logical :: has_export

         has_export = associated(find_export_var_spec(src_spec, var_spec%short_name))
         if (.not. has_export) then
            call unresolved%push_back(dst_pt%component_name // ':' // var_spec%short_name)
         end if

         _RETURN(_SUCCESS)
      end subroutine check_one

   end subroutine check_match_connection_unsatisfied

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
    subroutine graphbuilder_resolve_connections(this, unusable, unresolved_imports, &
         unsupported_characteristics, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(StringVector), optional, intent(out) :: unresolved_imports
      ! REQ scenario "Unregistered characteristic fails loudly": a
      ! distinguishable report, separate from unresolved_imports (which
      ! means "no export at all"), for a matched export/import pair whose
      ! mismatch has no registered extension provider
      ! (mapl_ExtensionResolution_mod).
      type(StringVector), optional, intent(out) :: unsupported_characteristics
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: graph
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(StringVector) :: unresolved
      type(StringVector) :: unsupported

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
   subroutine resolve_match_connection(this, conn, unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
      type(StringVector), intent(inout) :: unresolved
      type(StringVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(ComponentSpec), pointer :: dst_spec
      type(ComponentGraph), pointer :: this_graph
      type(DependencyNetworkId) :: net_id

      src_pt = conn%get_source()
      dst_pt = conn%get_destination()

      dst_spec => component_spec_for(this, dst_pt%component_name, _RC)

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
         logical :: has_export
         type(VariableSpec), pointer :: export_var_spec
         type(CharacteristicMap), target :: export_characteristics, import_characteristics
         type(CharacteristicId), allocatable :: mismatched(:)
         character(:), allocatable :: unsupported_characteristic
         character(:), allocatable :: materialization_failure

         import_node_id = get_or_make_local_node_id(this, dst_pt%component_name, &
              ESMF_STATEINTENT_IMPORT, var_spec%short_name, _RC)

         has_export = find_export_node_id(this, src_pt%component_name, var_spec%short_name, &
              export_node_id, export_var_spec, _RC)

         if (.not. has_export) then
            ! REQ scenario "Import with no matching export is left
            ! unresolved" - reported, not silently dropped.
            call unresolved%push_back(dst_pt%component_name // ':' // var_spec%short_name)
            _RETURN(_SUCCESS)
         end if

         export_characteristics = build_characteristics(export_var_spec, _RC)
         import_characteristics = build_characteristics(var_spec, _RC)
         mismatched = find_mismatched_characteristics(export_characteristics, import_characteristics)

         if (size(mismatched) == 0) then
            ! REQ-EXT-003 no-op case: matches exactly, wire directly.
            call this_graph%add_dependency(net_id, export_node_id, import_node_id, _RC)
            _RETURN(_SUCCESS)
         end if

         ! REQ-EXT-001/005: mismatch - delegate to the extension-reuse
         ! capability (mapl_ExtensionResolution_mod) rather than wiring
         ! the mismatched pair directly.
         call find_or_build_extension_chain(this_graph, net_id, export_node_id, &
              export_characteristics, import_characteristics, mismatched, &
              final_node_id, unsupported_characteristic, _RC)

         if (unsupported_characteristic /= '') then
            ! spec "Unregistered characteristic fails loudly" -
            ! distinguishable from "import has no matching export."
            call unsupported%push_back(dst_pt%component_name // ':' // var_spec%short_name // &
                 ':' // unsupported_characteristic)
            _RETURN(_SUCCESS)
         end if

         ! extension-registry-visibility change, task groups 1-3: chain
         ! *structure* above is unconditional (unaffected by the gate,
         ! exactly as 3c left it) - only the real FieldCreate-based
         ! payload materialization step is gated. Off (the default): the
         ! extension item keeps 3c's own unallocated placeholder payload,
         ! zero behavior change from today.
         if (materialize_extensions_enabled()) then
            call try_materialize_field_extension(this, src_pt%component_name, export_var_spec, var_spec, &
                 this_graph, final_node_id, materialization_failure, _RC)
            if (materialization_failure /= '') then
               ! Distinguishable from both "no matching export" and
               ! "unregistered characteristic fails loudly" above (spec
               ! "A non-field item class fails explicitly").
               call unsupported%push_back(dst_pt%component_name // ':' // var_spec%short_name // &
                    ':' // materialization_failure)
               _RETURN(_SUCCESS)
            end if
         end if

         call this_graph%add_dependency(net_id, final_node_id, import_node_id, _RC)

         _RETURN(_SUCCESS)
      end subroutine resolve_one

   end subroutine resolve_match_connection

   ! Builds the CharacteristicMap for one VariableSpec directly from its
   ! own declared fields - deliberately not via
   ! VariableSpec%make_StateitemSpec/make_aspects (design.md Decisions:
   ! "Characteristic is the graph's own name for what legacy calls an
   ! Aspect, and is a deliberately independent design, not a reskin").
   ! A kind is included only when the underlying field is actually
   ! populated, mirroring "aspect present" gating.
    function build_characteristics(var_spec, rc) result(characteristics)
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc
      type(CharacteristicMap) :: characteristics

      integer :: status
      character(20) :: grid_id_buffer

      if (allocated(var_spec%units)) then
         call characteristics%insert(UNITS_CHARACTERISTIC_ID, UnitsCharacteristic(var_spec%units))
      end if
      if (allocated(var_spec%vertical_grid)) then
         ! VerticalGrid%get_id() returns an integer identity token;
         ! VerticalGridCharacteristic compares an opaque string
         ! signature (Characteristic.F90 get_signature contract), so it
         ! is rendered to text here rather than changing
         ! VerticalGridCharacteristic's own constructor to know about
         ! integer ids specifically.
         write(grid_id_buffer, '(I0)') var_spec%vertical_grid%get_id()
         call characteristics%insert(VERTICAL_GRID_CHARACTERISTIC_ID, VerticalGridCharacteristic(trim(grid_id_buffer)))
      end if

      _RETURN(_SUCCESS)
   end function build_characteristics

   logical function find_export_node_id(this, comp_name, short_name, export_node_id, export_var_spec, rc) result(found)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      character(*), intent(in) :: short_name
      type(NodeId), intent(out) :: export_node_id
      type(VariableSpec), pointer, intent(out) :: export_var_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: src_spec

      export_var_spec => null()
      src_spec => component_spec_for(this, comp_name, _RC)
      export_var_spec => find_export_var_spec(src_spec, short_name)
      found = associated(export_var_spec)
      if (.not. found) then
         _RETURN(_SUCCESS)
      end if

      export_node_id = get_or_make_local_node_id(this, comp_name, ESMF_STATEINTENT_EXPORT, short_name, _RC)

      _RETURN(_SUCCESS)
   end function find_export_node_id

   function find_export_var_spec(comp_spec, short_name) result(export_var_spec)
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

   ! ============================================================
   ! Task 4: Public ports and child proxies
   ! ============================================================
   ! (Public-port recording for this component's OWN items happens in
   ! advertise_one() above - task 4.1. This section is the child-proxy
   ! half of task 4, task 4.2, needed by connection resolution above.)

   logical function is_self(this, comp_name) result(self_ref)
      class(OuterMetaComponent), intent(in) :: this
      character(*), intent(in) :: comp_name

      self_ref = (comp_name == SELF_COMPONENT_NAME) .or. (comp_name == this%get_name())
   end function is_self

   ! REQ-GB-002 permits GraphBuilder this reach into a named child's own
   ! spec/graph; it is never exposed through OuterMetaComponent's
   ! general public API (REQ-HIER-005) - see
   ! get_child_component_spec()/get_child_component_graph()'s own
   ! interface comment in OuterMetaComponent.F90 for the visibility
   ! rationale.
   function component_spec_for(this, comp_name, rc) result(comp_spec)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      integer, optional, intent(out) :: rc
      type(ComponentSpec), pointer :: comp_spec

      integer :: status

      if (is_self(this, comp_name)) then
         comp_spec => this%get_component_spec()
      else
         comp_spec => this%get_child_component_spec(comp_name, _RC)
      end if

      _RETURN(_SUCCESS)
   end function component_spec_for

   ! extension-registry-visibility change (design.md Finding 5/Decisions
   ! "Geom/vgrid resolution order"): same is_self()-gated shape as
   ! component_spec_for() above, but returning the owning
   ! OuterMetaComponent object itself (not just its ComponentSpec) - the
   ! geom/vertical_grid component-wide defaults
   ! (has_geom()/get_geom()/get_vertical_grid()) live on OuterMetaComponent,
   ! not ComponentSpec. The child-name case delegates to
   ! OuterMetaComponent's own get_child_outer_meta() (REQ-GB-002
   ! carve-out, same as get_child_component_spec/get_child_component_graph)
   ! rather than re-deriving the get_child()->get_gridcomp()->
   ! get_outer_meta() reach here.
   function component_for(this, comp_name, rc) result(comp_meta)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      integer, optional, intent(out) :: rc
      class(OuterMetaComponent), pointer :: comp_meta

      integer :: status

      comp_meta => null()

      if (is_self(this, comp_name)) then
         comp_meta => this
         _RETURN(_SUCCESS)
      end if

      comp_meta => this%get_child_outer_meta(comp_name, _RC)

      _RETURN(_SUCCESS)
   end function component_for

   ! extension-registry-visibility change, task groups 2/3: when the
   ! materialization gate (mapl_ExtensionResolution_mod) is enabled,
   ! give a resolved units-mismatch extension chain's final item a real,
   ! allocated ESMF_Field for a Field-typed export/import pair (design.md
   ! Decisions - "Materialize a real Field natively via FieldCreate"), or
   ! report an explicit, distinguishable failure_reason (empty string on
   ! success) for anything this capability cannot materialize a payload
   ! for - a non-Field item class on either side (design.md Decisions
   ! "Scope limited to Field-typed items"), or an unresolvable geom
   ! (design.md Decisions "Geom/vgrid resolution order" case (c)).
   subroutine try_materialize_field_extension(this, export_comp_name, export_var_spec, import_var_spec, &
        graph, node_id, failure_reason, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      type(VariableSpec), intent(in) :: export_var_spec
      type(VariableSpec), intent(in) :: import_var_spec
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(:), allocatable, intent(out) :: failure_reason
      integer, optional, intent(out) :: rc

      integer :: status
      class(OuterMetaComponent), pointer :: export_meta
      type(ESMF_Geom) :: geom
      logical :: has_geom
      class(VerticalGrid), allocatable :: vgrid
      type(VerticalStaggerLoc) :: vert_staggerloc

      failure_reason = ''

      if (.not. (export_var_spec%itemType == MAPL_STATEITEM_FIELD)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(export_var_spec%itemType)
         _RETURN(_SUCCESS)
      end if
      if (.not. (import_var_spec%itemType == MAPL_STATEITEM_FIELD)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(import_var_spec%itemType)
         _RETURN(_SUCCESS)
      end if
      _ASSERT(allocated(import_var_spec%units), 'GraphBuilder: materialization reached with no import units - internal inconsistency')

      export_meta => component_for(this, export_comp_name, _RC)

      call resolve_export_geom(export_var_spec, export_meta, geom, has_geom, _RC)
      if (.not. has_geom) then
         ! design.md Decisions "Geom/vgrid resolution order" case (c):
         ! neither the VariableSpec nor the owning OuterMetaComponent has
         ! a concrete geom - distinguishable from both the "unsupported
         ! item class" case above and 3c's own "unregistered
         ! characteristic fails loudly."
         failure_reason = 'unresolved_geom'
         _RETURN(_SUCCESS)
      end if

      ! Vertical grid is optional (a 2D field legitimately has none,
      ! same as FieldCreate's own optional vgrid argument) - unlike geom,
      ! its absence is not a failure. When present, a vert_staggerloc is
      ! required alongside it (FieldCreate's own contract); default to
      ! CENTER when the export's own VariableSpec does not say otherwise
      ! (VariableSpec.F90's own make_VerticalGridAspect precedent).
      call resolve_export_vgrid(export_var_spec, export_meta, vgrid)
      if (allocated(vgrid)) then
         vert_staggerloc = VERTICAL_STAGGER_CENTER
         if (allocated(export_var_spec%vertical_stagger)) vert_staggerloc = export_var_spec%vertical_stagger

         call materialize_field_extension(graph, node_id, geom=geom, typekind=export_var_spec%typekind, &
              units=import_var_spec%units, ungridded_dims=export_var_spec%ungridded_dims, &
              vgrid=vgrid, vert_staggerloc=vert_staggerloc, _RC)
      else
         call materialize_field_extension(graph, node_id, geom=geom, typekind=export_var_spec%typekind, &
              units=import_var_spec%units, ungridded_dims=export_var_spec%ungridded_dims, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine try_materialize_field_extension

   ! design.md Decisions "Geom/vgrid resolution order": (a) the export's
   ! own VariableSpec%geom if explicitly allocated (HistoryCollection-
   ! style explicit override), else (b) the owning OuterMetaComponent's
   ! component-wide default via its existing public has_geom()/get_geom()
   ! accessors (the same fallback advertise_variable.F90 already applies
   ! to every VariableSpec), else (c) not found - no cross-component
   ! mirror propagation (that remains a distinct, future capability).
   subroutine resolve_export_geom(var_spec, owner, geom, found, rc)
      type(VariableSpec), intent(in) :: var_spec
      class(OuterMetaComponent), target, intent(inout) :: owner
      type(ESMF_Geom), intent(out) :: geom
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      integer :: status

      found = .false.

      if (allocated(var_spec%geom)) then
         geom = var_spec%geom
         found = .true.
         _RETURN(_SUCCESS)
      end if

      if (owner%has_geom()) then
         geom = owner%get_geom(_RC)
         found = .true.
      end if

      _RETURN(_SUCCESS)
   end subroutine resolve_export_geom

   ! Same priority order as resolve_export_geom() above, for the
   ! vertical grid - but unlike geom, "not found" here means "no vertical
   ! dimension" (a legitimate 2D field), not a failure; see
   ! try_materialize_field_extension()'s own comment. An explicit
   ! `vertical_stagger == VERTICAL_STAGGER_NONE` (the parser's own
   ! `vertical_dim_spec: NONE`, VariableSpec.F90) is this VariableSpec's
   ! own explicit declaration that the field has no vertical dimension
   ! at all - honored ahead of the component-wide default so a 2D field
   ! declared inside an otherwise-3D-capable component is not
   ! mistakenly given that component's own vertical grid.
   subroutine resolve_export_vgrid(var_spec, owner, vgrid)
      type(VariableSpec), intent(in) :: var_spec
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(VerticalGrid), allocatable, intent(out) :: vgrid

      class(VerticalGrid), pointer :: component_vgrid

      if (allocated(var_spec%vertical_stagger)) then
         if (var_spec%vertical_stagger == VERTICAL_STAGGER_NONE) return
      end if

      if (allocated(var_spec%vertical_grid)) then
         vgrid = var_spec%vertical_grid
         return
      end if

      component_vgrid => owner%get_vertical_grid()
      if (associated(component_vgrid)) vgrid = component_vgrid
   end subroutine resolve_export_vgrid

   ! Human-readable item-class name for an unsupported-item-class failure
   ! message (design.md Decisions "Scope limited to Field-typed items")
   ! - distinguishes which non-Field class was encountered, mirroring
   ! VariableSpec.F90's own %itemType%ot select-case precedent
   ! (make_ClassAspect).
   function itemtype_name(item_type) result(name)
      type(ESMF_StateItem_Flag), intent(in) :: item_type
      character(:), allocatable :: name

      select case (item_type%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         name = 'FIELD'
      case (MAPL_STATEITEM_FIELDBUNDLE%ot)
         name = 'FIELDBUNDLE'
      case (MAPL_STATEITEM_VECTOR%ot)
         name = 'VECTOR'
      case (MAPL_STATEITEM_VECTORBRACKET%ot)
         name = 'VECTORBRACKET'
      case (MAPL_STATEITEM_BRACKET%ot)
         name = 'BRACKET'
      case (MAPL_STATEITEM_STATE%ot)
         name = 'STATE'
      case (MAPL_STATEITEM_SERVICE%ot)
         name = 'SERVICE'
      case (MAPL_STATEITEM_EXPRESSION%ot)
         name = 'EXPRESSION'
      case default
         name = 'UNKNOWN'
      end select
   end function itemtype_name

   ! Resolves a (component, intent, short_name) reference to a NodeId in
   ! THIS component's own graph:
   !  - "<self>"/this's own name: the item's own NodeId (must already
   !    have been advertised - graphbuilder_advertise() runs first).
   !  - a named child: a cached-or-newly-created parent-local proxy
   !    StateItemNode (REQ-HIER-006), reusing one already created for the
   !    same child+item via the same resource-index mechanism used for
   !    advertised-item identity (module header - "Cross-component
   !    (parent/child) connections").
   function get_or_make_local_node_id(this, comp_name, state_intent, short_name, rc) result(node_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      type(ComponentGraph), pointer :: this_graph
      character(:), allocatable :: item_id_key
      character(:), allocatable :: pkey
      type(NodeId), pointer :: existing
      type(NodeId), pointer :: child_item_id
      type(ComponentGraph), pointer :: child_graph
      type(StateItemNode) :: proxy_node
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
      type(PortId) :: port_id

      this_graph => this%get_component_graph()
      item_id_key = item_key(state_intent, short_name)

      if (is_self(this, comp_name)) then
         existing => this_graph%get_resource_index(item_id_key)
         _ASSERT(associated(existing), 'GraphBuilder: connection references an item this component never advertised: ' // short_name)
         node_id = existing
         _RETURN(_SUCCESS)
      end if

      pkey = proxy_key(comp_name, state_intent, short_name)
      existing => this_graph%get_resource_index(pkey)
      if (associated(existing)) then
         node_id = existing
         _RETURN(_SUCCESS)
      end if

      child_graph => this%get_child_component_graph(comp_name, _RC)
      child_item_id => child_graph%get_resource_index(item_id_key)
      _ASSERT(associated(child_item_id), 'GraphBuilder: connection references an item child "' // comp_name // '" never advertised: ' // short_name)

      node_id = this_graph%next_node_id(_RC)
      proxy_node = StateItemNode(node_id, payload, revision)
      call this_graph%register_node(proxy_node, _RC)
      call this_graph%add_resource_index(pkey, node_id, _RC)

      port_id = this_graph%next_port_id(_RC)
      call this_graph%add_child_port_binding(port_id, node_id, _RC)

      _RETURN(_SUCCESS)
   end function get_or_make_local_node_id

   ! ============================================================
   ! Task 5: Validate and freeze
   ! ============================================================

   ! ComponentGraph%freeze() already validates before transitioning
   ! lifecycle state (ComponentGraph.F90 graph_freeze: validate() is
   ! called first, and the frozen flag is only set on success) - this is
   ! a thin, directly-testable wrapper, not a reimplementation.
   subroutine graphbuilder_freeze(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph

      graph => this%get_component_graph()
      call graph%freeze(_RC)

      _RETURN(_SUCCESS)
   end subroutine graphbuilder_freeze

   ! ============================================================
   ! Task 6: Visualization enrichment (visualization-enrichment-layer
   ! change, REQ-VIZ-004/015)
   ! ============================================================

   ! Builds a NodeId -> label lookup for THIS component's own local
   ! graph only (design.md Goals/Non-Goals): a component's own
   ! advertised items are labeled by their own short_name (read from
   ! ComponentSpec%var_specs, the same iteration graphbuilder_advertise
   ! already performs); each cached child-proxy node in this
   ! component's own graph is labeled "<child_name>:<short_name>" and
   ! marked as a proxy, read from that child's own already-published
   ! ComponentSpec%var_specs (the same framework-internal carve-out
   ! get_or_make_local_node_id already uses, REQ-GB-002) - never the
   ! child's own graph/NodeId/DependencyNetwork. A child item never
   ! actually connected has no cached proxy, so
   ! graph%get_resource_index(proxy_key(...)) simply returns
   ! unassociated and that item is left out of the map - not an error
   ! (design.md Decisions, "Enrichment builds the map by walking
   ! var_specs").
   function graphbuilder_build_label_map(this, rc) result(label_map)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(NodeIdLabelMap) :: label_map

      integer :: status
      integer :: i, num_children
      type(ComponentGraph), pointer :: graph
      type(ComponentSpec), pointer :: comp_spec, child_spec
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      character(:), allocatable :: child_name
      character(:), allocatable :: key
      type(NodeId), pointer :: found_id

      graph => this%get_component_graph()
      comp_spec => this%get_component_spec()

      ! This component's own advertised items (design.md - "walking
      ! var_specs", not reversing the resource index).
      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            key = item_key(var_spec%state_intent, var_spec%short_name)
            found_id => graph%get_resource_index(key)
            if (associated(found_id)) then
               call label_map%insert(found_id, NodeLabel(var_spec%short_name, is_proxy=.false.))
            end if
         end do
      end associate

      ! Each child's cached proxy nodes, if any, in THIS component's
      ! own graph (never the child's own graph - design.md "Enrichment
      ! layer does not cross into a child's own graph").
      num_children = this%get_num_children()
      do i = 1, num_children
         child_name = this%get_child_name(i, _RC)
         child_spec => this%get_child_component_spec(child_name, _RC)

         associate (ce => child_spec%var_specs%ftn_end())
            iter = child_spec%var_specs%ftn_begin()
            do while (iter /= ce)
               call iter%next()
               var_spec => iter%of()
               key = proxy_key(child_name, var_spec%state_intent, var_spec%short_name)
               found_id => graph%get_resource_index(key)
               if (associated(found_id)) then
                  call label_map%insert(found_id, &
                       NodeLabel(child_name // ':' // var_spec%short_name, is_proxy=.true.))
               end if
            end do
         end associate
      end do

      _RETURN(_SUCCESS)
   end function graphbuilder_build_label_map

   ! ============================================================
   ! Lifecycle-hook wrappers (task 2.3 / 3.4)
   ! ============================================================
   !
   ! design.md Decisions ("Invocation point"): GraphBuilder calls are
   ! ADDITIONAL to the existing legacy advertise/connect calls, never a
   ! replacement, and MUST NOT alter initialize_advertise.F90's or
   ! initialize_accept_transfer.F90's own error propagation. The
   ! procedures above use the normal MAPL rc/status-check convention (so
   ! they remain directly unit-testable, task 6, with ordinary pass/fail
   ! semantics); these three wrappers are what actually get called from
   ! those lifecycle hooks - they catch any GraphBuilder-internal failure
   ! locally and report it through the component's own logger rather than
   ! propagating it, so a GraphBuilder defect can never break existing
   ! MAPL component initialization (design.md Risks/Trade-offs).
   !
   ! These three are reached from their call sites only via the public
   ! GraphBuilder type's bindings of the same name minus the
   ! graphbuilder_ prefix (graphbuilder-code-quality-cleanup change),
   ! e.g. `type(GraphBuilder) :: gb; call gb%run_advertise_hook(this)`.
   !
   ! Called from initialize_advertise.F90 (GENERIC_INIT_ADVERTISE, mirrors
   ! self_advertise() + activate()):
   !   graphbuilder_run_advertise_hook -> gb%run_advertise_hook - creates
   !     StateItemNodes
   !   graphbuilder_run_activate_hook -> gb%run_activate_hook -
   !     unresolved-imports report only
   ! Called from initialize_accept_transfer.F90 (GENERIC_INIT_ACCEPT_TRANSFER,
   ! mirrors connect()):
   !   graphbuilder_run_connect_hook -> gb%run_connect_hook - real
   !     proxies/edges, then freeze

   subroutine graphbuilder_run_advertise_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status

      call graphbuilder_advertise(this, status)
      call report_if_failed(this, 'advertise', status)
   end subroutine graphbuilder_run_advertise_hook

   subroutine graphbuilder_run_activate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      type(StringVector) :: unresolved
      integer :: i
      character(:), pointer :: unresolved_item
      class(Logger), pointer :: lgr

      call graphbuilder_check_unsatisfied_imports(this, unresolved_imports=unresolved, rc=status)
      call report_if_failed(this, 'check_unsatisfied_imports', status)

      lgr => this%get_logger()
      do i = 1, unresolved%size()
         unresolved_item => unresolved%of(i)
         call lgr%warning( &
              'GraphBuilder: no ordinary-match export found for unresolved import %a', unresolved_item)
      end do
   end subroutine graphbuilder_run_activate_hook

   subroutine graphbuilder_run_connect_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      integer :: status
      type(StringVector) :: unsupported
      integer :: i
      character(:), pointer :: unsupported_item
      class(Logger), pointer :: lgr

      call graphbuilder_resolve_connections(this, unsupported_characteristics=unsupported, rc=status)
      call report_if_failed(this, 'resolve_connections', status)

      lgr => this%get_logger()
      do i = 1, unsupported%size()
         unsupported_item => unsupported%of(i)
         call lgr%warning( &
              'GraphBuilder: no registered extension provider for mismatched characteristic %a', &
              unsupported_item)
      end do

      call graphbuilder_freeze(this, status)
      call report_if_failed(this, 'freeze', status)
   end subroutine graphbuilder_run_connect_hook

   subroutine report_if_failed(this, step_name, status)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: step_name
      integer, intent(in) :: status

      class(Logger), pointer :: lgr

      if (status /= 0) then
         lgr => this%get_logger()
         call lgr%warning( &
              'GraphBuilder: %a step failed with rc=%i0 - continuing without graph representation for this step', &
              step_name, status)
      end if
   end subroutine report_if_failed

end module mapl_GraphBuilder_mod
