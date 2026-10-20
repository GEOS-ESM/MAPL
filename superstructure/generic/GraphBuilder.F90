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
!
! Horizontal geometry (openspec/changes/horizontal-geometry-graph-state-
! item, roadmap Phase 4e, docs/graph/spec/13-geometry-and-vertical-
! grids.md REQ-GEO-001/002/003): unlike ordinary items, a component's
! horizontal geometry is not declared as a VariableSpec - GeometrySpec
! (superstructure/generic/specs/GeometrySpec.F90) already fully resolves
! which geometry a component ends up with (GEOMETRY_PROVIDER/
! GEOMETRY_FROM_PARENT/GEOMETRY_FROM_CHILD), via initialize_geom_a.F90/
! initialize_geom_b.F90 - GENERIC_INIT_PHASE_SEQUENCE
! (enums/GenericPhases.F90) runs GEOM_A/GEOM_B strictly AFTER ADVERTISE,
! not before it (openspec/changes/phase-10a-state-materialization found
! and corrected this header's own earlier, wrong claim of the reverse
! order - this%has_geom() is not yet true at ADVERTISE time for a
! GEOMETRY_PROVIDER component in a real lifecycle). The geometry hook
! below is therefore called from initialize_geom_b.F90, not
! initialize_advertise.F90 - see that file's own comment for the full
! story - so that, across the whole hierarchy, every component's own
! geom (own-provider or inherited-from-parent) is already real by the
! time the hook runs. graphbuilder_run_geometry_hook does not re-resolve
! that decision; it
! gives it graph-native STRUCTURE, once it is already known:
!   - graphbuilder_advertise_geometry(): if this%has_geom(), wraps
!     this%get_geom() as a geometry-proxy GraphStateItem
!     (mapl_GeomProxyField_mod) and registers it as an ordinary
!     StateItemNode in this component's own local graph, indexed under
!     the reserved GEOMETRY_ITEM_NAME key - deliberately NOT inserted
!     into ComponentSpec%var_specs (unlike ordinary items), so it is
!     structurally unreachable from self_advertise/
!     StateRegistry%add_to_states and can never leak into a user-facing
!     state (REQ-GEO-003) - this is what makes "declare it as an
!     ordinary VariableSpec" (an earlier, abandoned draft of this
!     design) unnecessary: item_key()/get_or_make_local_node_id() key
!     off (state_intent, short_name) strings, not off VariableSpec
!     identity, so reusing that machinery for a non-VariableSpec item
!     needs no VariableSpec at all.
!   - graphbuilder_resolve_geometry(): gives the ALREADY-resolved
!     parent/child relationship a real dependency edge, reusing
!     get_or_make_local_node_id() unmodified for the "receives from a
!     named child" pull direction (structurally identical to an
!     ordinary cross-boundary MatchConnection), and a new, symmetric
!     push-direction helper (push_geometry_to_child, using the same
!     framework-internal get_child_component_graph() carve-out
!     REQ-GB-002 already establishes) for the "provides to a child"
!     direction, which pull-direction machinery cannot express. Both
!     directions build their StateItemCharacteristicMap from
!     GeometryCharacteristic (mirroring PhysicalUnitsCharacteristic/
!     VerticalCoordinateCharacteristic exactly) and go through
!     find_mismatched_characteristics/
!     find_or_build_extension_chain like any other connection - a
!     mismatch here would mean GeometrySpec's own resolution and the
!     graph's independently-read this%get_geom_id() disagree, which
!     should be unreachable; it is asserted, not silently trusted.
!   - Entirely gated behind mapl_GraphMode_mod%graph_mode() reporting
!     something other than GRAPH_MODE_REGISTRY (design.md Decision D6;
!     openspec/changes/phase-10a-state-materialization, "3-way selector
!     replaces, rather than wraps, the old binary API"): a no-op for
!     every real production run today. Bottom-up call ordering
!     (initialize_geom_b.F90 recurses
!     into children before running this hook on itself) guarantees that
!     by the time a parent's own hook runs, every child already has its
!     own geometry node registered - required for both the pull and the
!     push direction above.
!------------------------------------------------------------------------------
module mapl_GraphBuilder_mod
   use esmf, only: ESMF_Field, ESMF_Geom, ESMF_STATEINTENT_EXPORT, ESMF_StateIntent_Flag, ESMF_STATEINTENT_IMPORT, ESMF_StateItem_Flag, operator(==)
   use esmf, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4
   use gFTL2_StringVector, only: StringVector
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId
   use mapl_CallbackInterfaceRegistry_mod, only: get_callback_interface
   use mapl_CallbackMethodBinding_mod, only: CallbackMethodBinding
   use mapl_CallbackStateBinding_mod, only: CallbackStateBinding
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap
   use mapl_PhysicalUnitsCharacteristic_mod, only: PhysicalUnitsCharacteristic
   use mapl_TypeKindCharacteristic_mod, only: TypeKindCharacteristic, label_to_typekind
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, PHYSICAL_UNITS_CHARACTERISTIC_KIND, &
        TYPE_KIND_CHARACTERISTIC_KIND, VERTICAL_COORDINATE_CHARACTERISTIC_KIND, operator(==), &
        ATTRIBUTES_CHARACTERISTIC_KIND, UNGRIDDED_DIMS_CHARACTERISTIC_KIND, &
        STANDARD_NAME_CHARACTERISTIC_KIND, QUANTITY_TYPE_CHARACTERISTIC_KIND, &
        CONSERVATION_CHARACTERISTIC_KIND, NORMALIZATION_CHARACTERISTIC_KIND, &
        ITEM_SHAPE_CHARACTERISTIC_KIND
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_ComponentSpec_mod, only: ComponentSpec
   use mapl_Connection_mod, only: Connection
   use mapl_ConnectionPt_mod, only: ConnectionPt
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_ExtensionMaterialization_mod, only: materialize_field_extension
   use mapl_GraphResolutionEntryVector_mod, only: GraphResolutionEntryVector
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_KeywordEnforcer_mod, only: KE => KeywordEnforcer
   use mapl_MatchConnection_mod, only: MatchConnection
   use mapl_SimpleConnection_mod, only: SimpleConnection
   use mapl_MultiState_mod, only: MultiState
   use mapl_NodeId_mod, only: NodeId, operator(==)
   use mapl_NodeIdLabelMap_mod, only: NodeIdLabelMap
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent
   use mapl_PortId_mod, only: PortId
   use mapl_StateItem_mod, only: MAPL_STATEITEM_BRACKET, MAPL_STATEITEM_EXPRESSION, MAPL_STATEITEM_FIELD, MAPL_STATEITEM_FIELDBUNDLE, MAPL_STATEITEM_SERVICE, MAPL_STATEITEM_STATE, MAPL_STATEITEM_VECTOR, MAPL_STATEITEM_VECTORBRACKET, MAPL_STATEITEM_WILDCARD
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_VariableSpec_mod, only: VariableSpec
   use mapl_VariableSpecVector_mod, only: VariableSpecVectorIterator, operator(/=)
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_VerticalStaggerLoc_mod, only: VERTICAL_STAGGER_CENTER, VerticalStaggerLoc
   use mapl_VirtualConnectionPt_mod, only: VirtualConnectionPt
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GraphBuilder
   public :: item_key
   ! openspec/changes/graphbuilder-submodule-split: made public (same
   ! reasoning as item_key above) because a gfortran-specific
   ! linkage limitation emits an ordinary (non-`module`) procedure
   ! defined directly in a private-by-default module's own CONTAINS
   ! with internal/static linkage when nothing marks it public - a
   ! submodule's own, separately-compiled .o cannot then link against
   ! it, even though F2008+ host-association rules say it should be
   ! reachable (NAG does not have this limitation; gfortran does).
   ! Each of these is called from at least one submodule, so each
   ! needed this fix once gfortran's own build was run against the
   ! split (tasks.md 9.4).
   public :: component_for
   public :: is_self
   public :: itemtype_name
   ! openspec/changes/horizontal-geometry-graph-state-item (Phase 4e):
   ! exposed the same way item_key already was, so tests (and
   ! any future code with legitimate reason to identify a geometry node)
   ! do not need to duplicate this module's key format.
   public :: GEOMETRY_ITEM_NAME

   ! openspec/changes/callback-wiring: exposed (like item_key/proxy_key
   ! above) so tests exercising this capability's own internal
   ! resolution steps directly - not just through the public
   ! GraphBuilder%resolve_connections()/check_unsatisfied_imports() entry
   ! points - do not need to duplicate this module's private logic.
   public :: QualifiedExportEntry
   public :: build_qualified_export_namespace
   public :: resolve_callback_import
   public :: materialize_callback_collection
   public :: build_callback_state_binding
   public :: build_callback_method_binding
   public :: invoke_callback_method

   ! openspec/changes/graph-builder-resolution-index: a shared,
   ! multi-level qualified-path resolution primitive, generalizing
   ! resolve_descendant (above, callback-wiring-only) for use by
   ! ordinary connection resolution too (proposal.md "What Changes").
   ! Public for the same gfortran cross-submodule-linkage reason as
   ! component_for/is_self above - not part of any user-facing API.
   public :: resolve_qualified_component
   public :: QualifiedExportIndexEntry
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 7.1,
   ! design.md D4: the History-support query - "given a qualified
   ! component path, return that component's own local resource index
   ! entries directly" - no push/propagation step. Composes
   ! resolve_qualified_component (path -> OuterMetaComponent) with the
   ! same resolved-through-resource_index, no-proxy per-item scan used
   ! by append_own_export_index_entries (below), scoped to the
   ! one resolved component itself (no recursion into its children).
   public :: query_component_exports

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.1: same
   ! gfortran cross-submodule-linkage reason as component_for/is_self
   ! above - called from GraphBuilder_LifecycleHooks_smod.F90, defined
   ! in GraphBuilder_ConnectionResolution_smod.F90.
   public :: UnresolvedImportEntry
   public :: graphbuilder_unresolved_import_node_ids
   public :: graphbuilder_run_propagate_hook
   public :: graphbuilder_run_export_propagate_hook

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
      ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.1/
      ! 6.2, design.md D3: for each direct child, mirrors any of that
      ! child's own still-unresolved imports into THIS component's own
      ! import-port table - the corrected replacement for 3b2 (no
      ! search, no proxy, no composite wrapper; see design.md D3 for
      ! the full reasoning and the two counts on which an earlier,
      ! superseded "search the subtree" draft was wrong). Called from
      ! initialize_advertise.F90, immediately after recurse() returns
      ! and before this component's own run_activate_hook.
      procedure, nopass :: run_propagate_hook => graphbuilder_run_propagate_hook
      ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.8:
      ! export propagation, symmetric to run_propagate_hook above but
      ! for exports instead of imports - unconditional (no "already
      ! resolved" gate, unlike import propagation: a direct child's own
      ! export is always propagated up, matching legacy's own
      ! propagate_exports_subregistry, which has no analogous gate
      ! either). Called from initialize_advertise.F90, at the same
      ! point legacy's own registry%propagate_exports call sits.
      procedure, nopass :: run_export_propagate_hook => graphbuilder_run_export_propagate_hook
      procedure, nopass :: run_activate_hook => graphbuilder_run_activate_hook
      procedure, nopass :: run_connect_hook => graphbuilder_run_connect_hook
      ! openspec/changes/hierarchy-wide-component-graph, tasks.md Group
      ! 9 (design.md D7): the GENERIC_INIT_GRAPH_FREEZE handler's own
      ! entry point (OuterMetaComponent/initialize_graph_freeze.F90) -
      ! called exactly once, only on the true hierarchy root (that
      ! file's own is_hierarchy_root check), performs ONE explicit,
      ! non-ESMF-recursive walk of the whole component tree, freezing
      ! every component's own ComponentGraph exactly once, THEN runs
      ! the hierarchy-wide convergence check (every remaining
      ! unresolved required import, anywhere in the hierarchy, is a
      ! hard failure - the same check graphbuilder_run_connect_hook used
      ! to run per-component immediately after its own per-component
      ! freeze call, now run exactly once, hierarchy-wide, after every
      ! component is frozen). graphbuilder_run_connect_hook itself no
      ! longer calls freeze - see that procedure's own header comment.
      procedure, nopass :: run_graph_freeze_hook => graphbuilder_run_graph_freeze_hook
      ! openspec/changes/horizontal-geometry-graph-state-item (Phase 4e):
      ! gated entirely behind mapl_GraphMode_mod%graph_mode() reporting
      ! something other than GRAPH_MODE_REGISTRY (design.md Decision D6)
      ! - a no-op for every real production run unless a test explicitly
      ! selects a non-registry graph mode.
      procedure, nopass :: run_geometry_hook => graphbuilder_run_geometry_hook
      ! openspec/changes/phase-10a-state-materialization: graph-native
      ! equivalent of StateRegistry%add_to_states - inserts each
      ! resolved, field-typed advertised item's real materialized field
      ! into a real ESMF_State under its own short name. Only a real
      ! producer when graph_mode() == GRAPH_MODE_GRAPH; a no-op
      ! otherwise (design.md "Graph-native state population"). Directly
      ! rc-returning (like advertise/resolve_connections above), for
      ! direct pFUnit coverage; run_populate_state_hook below is what
      ! the real GENERIC_INIT_ACCEPT_TRANSFER call site actually uses.
      procedure, nopass :: populate_state => graphbuilder_populate_state
      procedure, nopass :: run_populate_state_hook => graphbuilder_run_populate_state_hook
      ! openspec/changes/phase-10d-ordinary-phase-method-node:
      ! construction only - see graphbuilder_build_run_phase_node's own
      ! header comment. No invocation hook exists; do not add one
      ! without first resolving the blockers that comment names.
      procedure, nopass :: build_run_phase_node => graphbuilder_build_run_phase_node
      ! openspec/changes/graph-child-state-population, design.md D7:
      ! re-propagates a sibling connection's extension-chain transform
      ! after a run phase completes, via a plain ComponentGraph%update()
      ! pull - never a MethodGraphNode, never a second driver%run()
      ! call (REQ-NODE-007's own architectural guarantee). Called from
      ! run_user.F90, immediately after the legacy run call. A no-op
      ! outside GRAPH_MODE_GRAPH.
      procedure, nopass :: run_pull_hook => graphbuilder_run_pull_hook
   end type GraphBuilder

   ! Matches StateRegistry_Hierarchy_smod's own SELF sentinel
   ! (superstructure/generic/registry/StateRegistry_Hierarchy_smod.F90):
   ! a ConnectionPt component_name of "<self>" always refers to the
   ! component the connection was declared on.
   character(*), parameter :: SELF_COMPONENT_NAME = '<self>'

   ! openspec/changes/horizontal-geometry-graph-state-item (Phase 4e):
   ! reserved item name for a component's own resolved horizontal
   ! geometry, indexed via the same item_key()/get_or_make_local_node_id
   ! machinery ordinary advertised items use, but never inserted into
   ! ComponentSpec%var_specs (design.md D5-revised - see module header
   ! "Horizontal geometry" note) so it never reaches self_advertise/
   ! StateRegistry%add_to_states and cannot leak into user-facing states
   ! (REQ-GEO-003).
   character(*), parameter :: GEOMETRY_ITEM_NAME = 'MAPL_Geometry'

   ! Per-match callback signature for for_each_matching_import() below -
   ! same shape/precedent as OuterMetaComponent's own I_child_op
   ! (superstructure/generic/OuterMetaComponent.F90), passed an internal
   ! (CONTAINS-nested) procedure by each caller so it can host-associate
   ! whatever extra context (this, src_pt, unresolved, ...) it needs
   ! without threading it through this interface.
   !
   ! openspec/changes/graph-builder-stateitem-resolution, design.md
   ! Decisions: changed from `subroutine(var_spec, rc)` to
   ! `subroutine(node, rc)` - for_each_matching_import's own rewritten
   ! body (tasks.md 3.1) iterates the destination ComponentGraph's
   ! import ports, not dst_spec%var_specs, so it only ever has a
   ! StateItemNode in hand, never a VariableSpec. Private to
   ! mapl_GraphBuilder_mod - a submodule-internal interface change, not
   ! a public API change.
   abstract interface
      subroutine I_match_op(node, rc)
         import StateItemNode
         class(StateItemNode), target, intent(in) :: node
         integer, optional, intent(out) :: rc
      end subroutine I_match_op
   end interface

   ! openspec/changes/callback-wiring: one entry in the flattened
   ! qualified-export namespace (build_qualified_export_namespace,
   ! below) - comp_path is '' for the namespace root's own export, else
   ! the '/'-joined descendant path (e.g. 'CHEM/DU') the export was
   ! found under (design.md Decision 2 - a plain character key, no
   ! VirtualConnectionPt dependency for the storage shape itself).
   ! Public (not an opaque handle) since matching and interface-
   ! conformance both need the underlying VariableSpec, and resolving a
   ! match into a real NodeId needs comp_path.
   type :: QualifiedExportEntry
      character(:), allocatable :: qualified_name
      character(:), allocatable :: comp_path
      type(VariableSpec) :: var_spec
   end type QualifiedExportEntry

   ! openspec/changes/graph-builder-resolution-index, design.md Decisions:
   ! one entry in a resolved-export scan's own
   ! flattened result - a (qualified_path, owner, node_id) tuple,
   ! deliberately NOT carrying a VariableSpec/var_spec reference (unlike
   ! QualifiedExportEntry above), since this primitive's whole point is
   ! to NOT re-introduce a var_spec dependency into resolution's own
   ! vocabulary (design.md Decisions). qualified_path is '' for the
   ! enumeration root's own export, else the '/'-joined descendant path
   ! the export was found under - same convention as
   ! QualifiedExportEntry%qualified_name above.
   type :: QualifiedExportIndexEntry
      character(:), allocatable :: qualified_path
      type(OuterMetaComponent), pointer :: owner => null()
      type(NodeId) :: node_id
   end type QualifiedExportIndexEntry

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.1,
   ! design.md D3: one entry in
   ! graphbuilder_unresolved_import_node_ids's own structural scan
   ! result - a component's own still-unresolved import, identified by
   ! short_name (for GraphResolutionEntry-shaped reporting elsewhere)
   ! and its own already-real node_id (what graphbuilder_run_propagate_hook
   ! actually mirrors - the same NodeId, no copy).
   type :: UnresolvedImportEntry
      character(:), allocatable :: short_name
      type(NodeId) :: node_id
   end type UnresolvedImportEntry

   ! Argument names are short, framework-declared identifiers - matches
   ! mapl_MethodInvocation_mod's own MAX_ARGUMENT_NAME_LEN precedent.
   integer, parameter :: MAX_CALLBACK_ARGUMENT_NAME_LEN = 128

   ! ---- Interface block: one module function/subroutine declaration
   ! per procedure whose body moved to a submodule (design.md D1/D3) ----

   interface

      ! Advertising procedures (GraphBuilder_Advertising_smod.F90)
   module subroutine graphbuilder_advertise(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_advertise
   module subroutine advertise_one(graph, var_spec, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc

   end subroutine advertise_one

      ! Ordinary connection resolution procedures (GraphBuilder_ConnectionResolution_smod.F90)
   module subroutine graphbuilder_check_unsatisfied_imports(this, unusable, unresolved_imports, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved_imports
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_check_unsatisfied_imports
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.1,
   ! design.md D3 (corrected twice during implementation - see both
   ! correction notes in tasks.md 6.1 and this function's own body
   ! comment, GraphBuilder_ConnectionResolution_smod.F90, for the full
   ! history): "resolved" cannot be read off dependency-edge wiring
   ! (no edge exists anywhere, for any component, at the ADVERTISE-time
   ! point this runs - edges are only added later, at ACCEPT_TRANSFER,
   ! by graphbuilder_resolve_connections) nor off
   ! graphbuilder_check_unsatisfied_imports(C) alone (that procedure
   ! only ever inspects connections C itself declares - a direct
   ! parent's OWN ordinary connection naming C directly, the single
   ! most common connection shape in the whole codebase, is invisible
   ! to it, which would silently mirror an already-resolved import and
   ! risk a real REQ-DEP-008a two-producer violation once some
   ! ancestor's own connection also names `this_parent` directly for
   ! the same short name). An import in `child_meta`'s own import-port
   ! table counts as unresolved iff NEITHER (a) a MatchConnection
   ! declared on `child_meta` itself, whose destination resolves (via
   ! resolve_qualified_component) back to `child_meta` itself, NOR (b)
   ! a MatchConnection declared on `this_parent` itself, whose
   ! destination resolves directly to `child_meta` - finds a matching
   ! source export for it (the same per-item check
   ! check_match_connection_unsatisfied's own check_one already makes,
   ! reused here as a POSITIVE match collector instead of a negative
   ! one). `child_meta` MUST be one of `this_parent`'s own direct
   ! children. Read-only; creates no graph structure.
   module function graphbuilder_unresolved_import_node_ids(this_parent, child_meta, rc) result(unresolved)
      class(OuterMetaComponent), target, intent(inout) :: this_parent
      class(OuterMetaComponent), target, intent(inout) :: child_meta
      integer, optional, intent(out) :: rc
      type(UnresolvedImportEntry), allocatable :: unresolved(:)

   end function graphbuilder_unresolved_import_node_ids
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.1/6.2,
   ! design.md D3: for each direct child C of `this` (unconditionally -
   ! no connection naming C is required), mirrors each of C's own
   ! still-unresolved imports (graphbuilder_unresolved_import_node_ids,
   ! above, invoked on C's behalf - the same "parent reaches into
   ! child" framework-internal carve-out get_child_component_graph/
   ! get_child_component_spec already document) into `this`'s own
   ! import-port table, under C's own already-real NodeId - no new
   ! node, no proxy, no composite wrapper. Called from
   ! initialize_advertise.F90, immediately after run_activate_hook (not
   ! before - legacy's own propagate_unsatisfied_imports call site sits
   ! after process_connections/activate() for the same reason: a
   ! child's own is_active()-equivalent state must already reflect that
   ! child's own just-completed activate-time resolution before this
   ! component's own mirroring step reads it, or every import resolved
   ! at the child's own level would be needlessly mirrored upward too).
    module subroutine graphbuilder_run_propagate_hook(this)
       class(OuterMetaComponent), target, intent(inout) :: this

    end subroutine graphbuilder_run_propagate_hook
    ! openspec/changes/hierarchy-wide-component-graph, tasks.md 6.8
    ! (discovered as a required completion of 6.1/6.2 while chasing a
    ! real-lifecycle regression in history_1/history_wildcard/
    ! statistics_real - see this subroutine's own body comment,
    ! GraphBuilder_ConnectionResolution_smod.F90, for the full
    ! postmortem): for each direct child C of `this` (unconditionally),
    ! propagates each of C's own export-port entries into `this`'s own
    ! export-port table, under C's own already-real NodeId - no new
    ! node, exactly like run_propagate_hook's import-side mirroring.
    ! Unlike import propagation, the propagated entry's own effective
    ! NAME is additionally qualified with C's own name
    ! (C_name/short_name) - the exact pattern legacy's own
    ! propagate_exports_virtual_pt already establishes
    ! (StateRegistry_Propagation_smod.F90: "comp_name == ''" -> qualify
    ! with the immediate child's own name; already-qualified -> pass
    ! through UNCHANGED at every further ancestor level, per the user's
    ! own confirmation: "Export E from component A becomes export A/E
    ! in the parent component P, but stays A/E in the grandparent
    ! component"). This qualified name is NEVER written onto the
    ! underlying StateItemNode's own short_name (that identity field
    ! belongs to the component that actually advertised it, and is
    ! never rewritten) - it is recorded only in `this`'s own
    ! export-port table (ComponentGraph%add_export_port's own
    ! qualified_name argument, GraphPortRegistry's own header comment),
    ! which is exactly what find_export_node (ordinary resolution's own
    ! read path) now matches against instead of the node's own real
    ! short_name directly.
    module subroutine graphbuilder_run_export_propagate_hook(this)
       class(OuterMetaComponent), target, intent(inout) :: this

    end subroutine graphbuilder_run_export_propagate_hook
   module subroutine for_each_matching_import(dst_graph, dst_pt, op, rc)
      type(ComponentGraph), target, intent(in) :: dst_graph
      type(ConnectionPt), intent(in) :: dst_pt
      procedure(I_match_op) :: op
      integer, optional, intent(out) :: rc

   end subroutine for_each_matching_import
   module subroutine check_match_connection_unsatisfied(this, conn, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      integer, optional, intent(out) :: rc

   end subroutine check_match_connection_unsatisfied
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

    end subroutine graphbuilder_resolve_connections
   module subroutine resolve_match_connection(this, conn, unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

   end subroutine resolve_match_connection
   logical module function find_export_node_id(this, comp_name, short_name, export_node_id, export_node, rc) result(found)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      character(*), intent(in) :: short_name
      type(NodeId), intent(out) :: export_node_id
      class(StateItemNode), pointer, intent(out) :: export_node
      integer, optional, intent(out) :: rc

   end function find_export_node_id
   module subroutine resolve_inout_destination(this, src_pt, dst_pt, dst_node, effective_src_name, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), target, intent(in) :: dst_node
      character(*), intent(in) :: effective_src_name
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      integer, optional, intent(out) :: rc

   end subroutine resolve_inout_destination
   module subroutine try_alias_export_field(this, export_comp_name, export_short_name, &
        graph, node_id, found, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      character(*), intent(in) :: export_short_name
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

   end subroutine try_alias_export_field

      ! Public ports and child proxies procedures (GraphBuilder_PortsAndProxies_smod.F90)
   module subroutine resolve_export_geom(export_item, owner, geom, found, rc)
      type(GraphStateItem), intent(in) :: export_item
      class(OuterMetaComponent), target, intent(inout) :: owner
      type(ESMF_Geom), intent(out) :: geom
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

   end subroutine resolve_export_geom
   module subroutine resolve_export_vgrid(export_item, owner, vgrid)
      type(GraphStateItem), intent(in) :: export_item
      class(OuterMetaComponent), target, intent(inout) :: owner
      class(VerticalGrid), allocatable, intent(out) :: vgrid

   end subroutine resolve_export_vgrid

      ! Horizontal geometry procedures (GraphBuilder_Geometry_smod.F90)
   module subroutine graphbuilder_run_geometry_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

   end subroutine graphbuilder_run_geometry_hook
   module subroutine graphbuilder_advertise_geometry(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_advertise_geometry
   module subroutine graphbuilder_resolve_geometry(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_resolve_geometry
   module subroutine resolve_geometry_from_child(this, provider_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: provider_name
      integer, optional, intent(out) :: rc

   end subroutine resolve_geometry_from_child
   module subroutine push_geometry_to_child(this, child_name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

   end subroutine push_geometry_to_child

      ! Validate and freeze procedures (GraphBuilder_Freeze_smod.F90)
   module subroutine graphbuilder_freeze(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_freeze
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md Group 9:
   ! see the type-bound procedure's own declaration-site comment above.
   module subroutine graphbuilder_run_graph_freeze_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

   end subroutine graphbuilder_run_graph_freeze_hook

      ! Graph-native state population procedures (GraphBuilder_StatePopulation_smod.F90)
   module subroutine graphbuilder_populate_state(this, multi_state, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_populate_state
   module subroutine populate_one(graph, net, multi_state, id_ptr, state_intent, short_name, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      type(MultiState), intent(inout) :: multi_state
      type(NodeId), pointer, intent(in) :: id_ptr
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc

   end subroutine populate_one
   module subroutine get_field_if_materialized(graph, node_id, field, found, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(NodeId), intent(in) :: node_id
      type(ESMF_Field), intent(out) :: field
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

   end subroutine get_field_if_materialized
   module function find_materialized_predecessor(graph, net, start_id, found) result(field_node_id)
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      type(NodeId), intent(in) :: start_id
      logical, intent(out) :: found
      type(NodeId) :: field_node_id

   end function find_materialized_predecessor

      ! Re-propagation pull procedures (GraphBuilder_RunPhasePull_smod.F90)
   module subroutine graphbuilder_run_pull_hook(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_run_pull_hook

      ! Visualization enrichment procedures (GraphBuilder_Visualization_smod.F90)
   module function graphbuilder_build_label_map(this, rc) result(label_map)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(NodeIdLabelMap) :: label_map

   end function graphbuilder_build_label_map

      ! Callback wiring procedures (GraphBuilder_CallbackWiring_smod.F90)
   module function build_qualified_export_namespace(root, rc) result(entries)
      class(OuterMetaComponent), target, intent(inout) :: root
      integer, optional, intent(out) :: rc
      type(QualifiedExportEntry), allocatable :: entries(:)

   end function build_qualified_export_namespace
   recursive module subroutine collect_qualified_exports(owner, path_prefix, entries, rc)
      class(OuterMetaComponent), target, intent(inout) :: owner
      character(*), intent(in) :: path_prefix
      type(QualifiedExportEntry), allocatable, intent(inout) :: entries(:)
      integer, optional, intent(out) :: rc

   end subroutine collect_qualified_exports
   module function resolve_descendant(root, comp_path, rc) result(meta)
      class(OuterMetaComponent), target, intent(inout) :: root
      character(*), intent(in) :: comp_path
      integer, optional, intent(out) :: rc
      type(OuterMetaComponent), pointer :: meta

   end function resolve_descendant
   module function resolve_qualified_export_node_id(this, src_component_name, entry, rc) result(node_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: src_component_name
      type(QualifiedExportEntry), intent(in) :: entry
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

   end function resolve_qualified_export_node_id
   module function export_conforms_to_interface(var_spec, expected_iface, reason) result(conforms)
      type(VariableSpec), target, intent(in) :: var_spec
      type(CallbackInterface), target, intent(in) :: expected_iface
      character(:), allocatable, intent(out) :: reason
      logical :: conforms

   end function export_conforms_to_interface
   module function materialize_callback_collection(this, src_pt, matches, rc) result(collection_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(QualifiedExportEntry), intent(in) :: matches(:)
      integer, optional, intent(out) :: rc
      type(NodeId) :: collection_id

   end function materialize_callback_collection
   module function build_callback_state_binding(this, src_component_name, entry, expected_interface_id, &
        expected_iface, rc) result(binding)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: src_component_name
      type(QualifiedExportEntry), intent(in) :: entry
      type(CallbackInterfaceId), intent(in) :: expected_interface_id
      type(CallbackInterface), intent(in) :: expected_iface
      integer, optional, intent(out) :: rc
      type(CallbackStateBinding) :: binding

   end function build_callback_state_binding
   module function build_callback_method_binding(this, iface, method_name, state_binding, &
        provider_get_ids, provider_put_ids, rc) result(binding)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: method_name
      type(CallbackStateBinding), target, intent(in) :: state_binding
      type(StateItemMemberMap), intent(in) :: provider_get_ids
      type(StateItemMemberMap), intent(in) :: provider_put_ids
      integer, optional, intent(out) :: rc
      type(CallbackMethodBinding) :: binding

   end function build_callback_method_binding
   module subroutine wire_callback_argument(graph, network_id, source_id, target_id, rc)
      type(ComponentGraph), target, intent(inout) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: source_id
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc

   end subroutine wire_callback_argument
   module subroutine invoke_callback_method(graph, binding, rc)
      class(ComponentGraph), target, intent(inout) :: graph
      type(CallbackMethodBinding), target, intent(in) :: binding
      integer, optional, intent(out) :: rc

   end subroutine invoke_callback_method
   module subroutine pull_callback_get_arguments(graph, network_id, get_bindings, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetworkId), intent(in) :: network_id
      type(StateItemMemberMap), target, intent(in) :: get_bindings
      integer, optional, intent(out) :: rc

   end subroutine pull_callback_get_arguments
   module subroutine advance_callback_put_arguments(graph, put_bindings, rc)
      class(ComponentGraph), target, intent(in) :: graph
      type(StateItemMemberMap), target, intent(in) :: put_bindings
      integer, optional, intent(out) :: rc

   end subroutine advance_callback_put_arguments

      ! Ordinary-phase MethodGraphNode construction procedures (GraphBuilder_RunPhaseNode_smod.F90)
   module subroutine graphbuilder_build_run_phase_node(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_build_run_phase_node

      ! Lifecycle-hook wrapper procedures (GraphBuilder_LifecycleHooks_smod.F90)
   module subroutine graphbuilder_run_advertise_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

   end subroutine graphbuilder_run_advertise_hook
   module subroutine graphbuilder_run_activate_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

   end subroutine graphbuilder_run_activate_hook
    module subroutine graphbuilder_run_connect_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

    end subroutine graphbuilder_run_connect_hook
   module subroutine graphbuilder_run_populate_state_hook(this, multi_state)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state

   end subroutine graphbuilder_run_populate_state_hook
   module subroutine assert_converged(unresolved, rc)
      type(GraphResolutionEntryVector), intent(in) :: unresolved
      integer, optional, intent(out) :: rc

   end subroutine assert_converged
   module subroutine report_if_failed(this, step_name, status)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: step_name
      integer, intent(in) :: status

   end subroutine report_if_failed

   end interface

   ! ---- Shared helpers, used across more than one submodule ----
   ! (openspec/changes/graphbuilder-submodule-split, design.md D2: every
   ! procedure below is called from more than one of the eleven
   ! GraphBuilder_*_smod.F90 submodules, so it stays directly
   ! CONTAINS-ed in this module, visible to every submodule via
   ! ordinary host association - same role StateRegistry.F90's own
   ! SELF parameter plays for its own submodule split.)

contains

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
   ! extension-registry-visibility change (design.md Finding 5/Decisions
   ! "Geom/vgrid resolution order"): is_self()-gated reach into a named
   ! component, returning the owning
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

   ! openspec/changes/graph-builder-resolution-index, design.md
   ! Decisions "resolve_qualified_component is resolve_descendant,
   ! relocated and renamed, with no algorithm change": walks path's own
   ! '/'-separated segments via get_child_outer_meta, one hop per
   ! segment, from root to the OuterMetaComponent the path's final
   ! segment names - generalizing component_for's own single-level
   ! "<self>"/direct-child dispatch to arbitrary depth. path's first
   ! segment is special-cased against is_self(root, segment) (mirroring
   ! component_for's own is_self short-circuit) before falling through
   ! to get_child_outer_meta, so a single-segment path equal to
   ! root%get_name() (or "<self>") resolves to root itself rather than
   ! attempting a child lookup for a child literally named that
   ! (design.md Decisions/Risks - the fixed gap versus
   ! resolve_descendant's own prior behavior, which always called
   ! get_child_outer_meta unconditionally on the first segment). path
   ! MUST be non-empty - "root itself" is reached by the is_self
   ! short-circuit on a single-segment path naming root, not by an
   ! empty path (same convention resolve_descendant's own callers
   ! already observe).
   function resolve_qualified_component(root, path, rc) result(meta)
      class(OuterMetaComponent), target, intent(inout) :: root
      character(*), intent(in) :: path
      integer, optional, intent(out) :: rc
      type(OuterMetaComponent), pointer :: meta

      integer :: status
      character(:), allocatable :: remaining, segment
      integer :: slash_pos

      remaining = path

      slash_pos = index(remaining, '/')
      if (slash_pos == 0) then
         segment = remaining
         remaining = ''
      else
         segment = remaining(1:slash_pos - 1)
         remaining = remaining(slash_pos + 1:)
      end if

      if (is_self(root, segment)) then
         meta => root
      else
         meta => root%get_child_outer_meta(segment, _RC)
      end if

      do while (len(remaining) > 0)
         slash_pos = index(remaining, '/')
         if (slash_pos == 0) then
            segment = remaining
            remaining = ''
         else
            segment = remaining(1:slash_pos - 1)
            remaining = remaining(slash_pos + 1:)
         end if
         meta => meta%get_child_outer_meta(segment, _RC)
      end do

      _RETURN(_SUCCESS)
   end function resolve_qualified_component

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 7.1,
   ! design.md D4: the per-item resolved-export scan factored out as its
   ! own primitive so query_component_exports (below) can reuse the exact
   ! same "resolved-through-resource_index, no proxy" scan for ONE
   ! component without pulling in any recursion into children.
   ! Appends owner's own resolved exports
   ! (one entry per declared export already present in owner's own
   ! resource index - an export never advertised, or advertised but not
   ! yet resolved, contributes no entry) to entries, in place.
   subroutine append_own_export_index_entries(owner, path_prefix, entries, rc)
      class(OuterMetaComponent), target, intent(inout) :: owner
      character(*), intent(in) :: path_prefix
      type(QualifiedExportIndexEntry), allocatable, intent(inout) :: entries(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: owner_graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      type(QualifiedExportIndexEntry) :: entry
      type(QualifiedExportIndexEntry), allocatable :: grown_entries(:)
      type(NodeId), pointer :: resolved_id
      integer :: n

      comp_spec => owner%get_component_spec()
      owner_graph => owner%get_component_graph()

      associate (e => comp_spec%var_specs%ftn_end())
         iter = comp_spec%var_specs%ftn_begin()
         do while (iter /= e)
            call iter%next()
            var_spec => iter%of()
            if (.not. (var_spec%state_intent == ESMF_STATEINTENT_EXPORT)) cycle

            resolved_id => owner_graph%get_resource_index(item_key(ESMF_STATEINTENT_EXPORT, var_spec%short_name))
            if (.not. associated(resolved_id)) cycle

            if (len(path_prefix) == 0) then
               entry%qualified_path = var_spec%short_name
            else
               entry%qualified_path = path_prefix // '/' // var_spec%short_name
            end if
            entry%owner => owner
            entry%node_id = resolved_id

            ! Same classic-ifort self-referencing array-constructor ICE
            ! as collect_qualified_exports (GraphBuilder_CallbackWiring_smod.F90)
            ! - grow via an explicit fresh temporary and move_alloc.
            n = size(entries)
            allocate(grown_entries(n + 1))
            if (n > 0) grown_entries(1:n) = entries
            grown_entries(n + 1) = entry
            call move_alloc(grown_entries, entries)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine append_own_export_index_entries

   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 7.1,
   ! design.md D4 ("Export visibility (History) is a query, not a
   ! push"): given a qualified component path, resolve that component
   ! (resolve_qualified_component, unmodified - the same descendant-only
   ! tree walk ordinary connection resolution already uses) and return
   ! ONLY that component's own local resource-index entries - no
   ! recursion into its children, and no push/propagation step of
   ! any kind. A hierarchy-wide consumer (e.g. History) walks the
   ! OuterMetaComponent tree itself, calling this once per component path
   ! it wants to inspect, exactly as D4 describes - this primitive does
   ! not do that walk on the consumer's behalf.
   !
   ! root is the hierarchy root resolve_qualified_component resolves
   ! path against (same convention/argument shape as that function);
   ! path follows resolve_qualified_
   ! component's own single-segment-or-"/"-separated-multi-segment
   ! convention, including resolving to root itself via is_self's
   ! "<self>"/root-name short-circuit.
   function query_component_exports(root, path, rc) result(entries)
      class(OuterMetaComponent), target, intent(inout) :: root
      character(*), intent(in) :: path
      integer, optional, intent(out) :: rc
      type(QualifiedExportIndexEntry), allocatable :: entries(:)

      integer :: status
      type(OuterMetaComponent), pointer :: target_meta

      target_meta => resolve_qualified_component(root, path, _RC)

      allocate(entries(0))
      call append_own_export_index_entries(target_meta, '', entries, _RC)

      _RETURN(_SUCCESS)
   end function query_component_exports

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

   ! REQ-CB-016: expands the connection's own declared source pattern
   ! (src_pt%v_pt) against the flattened qualified-export namespace
   ! rooted at src_pt%component_name (design.md Decision 1), keeping
   ! only matches that implement expected_interface_id's declared
   ! expected CallbackInterface (design.md Decision 4). rejected
   ! (optional) collects a human-readable reason string per rejected
   ! match (spec scenario "A matching export that does not implement the
   ! expected interface is rejected" - "the rejection is reported").
   ! Read-only: never mutates any ComponentGraph - safe to call from both
   ! the activate-time unsatisfied-check and the real connect-time
   ! resolution.
   !
   ! openspec/changes/graph-builder-stateitem-resolution: dst_var_spec
   ! replaced by expected_interface_id (a plain CallbackInterfaceId
   ! argument) - this procedure only ever read %callback_interface_id
   ! off it, never any other VariableSpec field. for_each_matching_
   ! import's own rewrite (task 3.1) only ever has a StateItemNode in
   ! hand at its call sites, which exposes get_callback_interface_id()
   ! directly - no VariableSpec reference is needed here either.
   function resolve_callback_import(this, src_pt, expected_interface_id, rejected, rc) result(matches)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(CallbackInterfaceId), intent(in) :: expected_interface_id
      type(StringVector), optional, intent(out) :: rejected
      integer, optional, intent(out) :: rc
      type(QualifiedExportEntry), allocatable :: matches(:)

      integer :: status
      class(OuterMetaComponent), pointer :: root_meta
      type(QualifiedExportEntry), allocatable :: namespace(:)
      type(VirtualConnectionPt) :: candidate_v_pt
      type(CallbackInterface) :: expected_iface
      type(StringVector) :: reject_reasons
      integer :: i
      integer :: n
      logical :: conforms
      character(:), allocatable :: reason
      type(QualifiedExportEntry), allocatable :: grown_matches(:)

      allocate(matches(0))

      root_meta => component_for(this, src_pt%component_name, _RC)
      namespace = build_qualified_export_namespace(root_meta, _RC)
      expected_iface = get_callback_interface(expected_interface_id, _RC)

      do i = 1, size(namespace)
         candidate_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, namespace(i)%qualified_name)
         if (.not. src_pt%v_pt%matches(candidate_v_pt)) cycle

         conforms = export_conforms_to_interface(namespace(i)%var_spec, expected_iface, reason)
         if (.not. conforms) then
            call reject_reasons%push_back(namespace(i)%qualified_name // ':' // reason)
            cycle
         end if

         ! Same classic-ifort self-referencing-array-constructor ICE as
         ! collect_qualified_exports above - grow via a fresh temporary
         ! and move_alloc instead of `matches = [matches, namespace(i)]`.
         n = size(matches)
         allocate(grown_matches(n + 1))
         grown_matches(1:n) = matches
         grown_matches(n + 1) = namespace(i)
         call move_alloc(grown_matches, matches)
      end do

      if (present(rejected)) rejected = reject_reasons

      _RETURN(_SUCCESS)
   end function resolve_callback_import



end module mapl_GraphBuilder_mod
