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
        TYPE_KIND_CHARACTERISTIC_KIND, VERTICAL_COORDINATE_CHARACTERISTIC_KIND, operator(==)
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
   use mapl_MultiState_mod, only: MultiState
   use mapl_NodeId_mod, only: NodeId
   use mapl_NodeIdLabelMap_mod, only: NodeIdLabelMap
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent
   use mapl_PortId_mod, only: PortId
   use mapl_StateItem_mod, only: MAPL_STATEITEM_BRACKET, MAPL_STATEITEM_EXPRESSION, MAPL_STATEITEM_FIELD, MAPL_STATEITEM_FIELDBUNDLE, MAPL_STATEITEM_SERVICE, MAPL_STATEITEM_STATE, MAPL_STATEITEM_VECTOR, MAPL_STATEITEM_VECTORBRACKET
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
   public :: component_spec_for
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 3.1/3.2:
   ! replaces get_or_make_local_node_id (removed along with proxy_key -
   ! no parent-local stand-in node is created anymore; this resolves
   ! directly to the real owning component's own real NodeId).
   public :: resolve_local_node_id
   public :: is_self
   public :: itemtype_name
   public :: try_materialize_field_extension
   ! openspec/changes/phase-11b-chain-intermediate-materialization: new
   ! per-link materialization helper replacing each of the four
   ! find_or_build_extension_chain call sites' own single final-node
   ! try_materialize_field_extension call (design.md D3/D4) - exposed
   ! the same way try_materialize_field_extension already is, for direct
   ! test coverage and submodule call-site use.
   public :: materialize_extension_chain
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
   public :: resolve_callback_destination
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
   public :: enumerate_qualified_exports
   public :: QualifiedExportIndexEntry

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

   ! openspec/changes/graph-builder-resolution-index, design.md Decisions
   ! "enumerate_qualified_exports generalizes collect_qualified_exports's
   ! own recursion": one entry in enumerate_qualified_exports's own
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
   module subroutine for_each_matching_import(dst_graph, dst_pt, op, rc)
      type(ComponentGraph), target, intent(in) :: dst_graph
      type(ConnectionPt), intent(in) :: dst_pt
      procedure(I_match_op) :: op
      integer, optional, intent(out) :: rc

   end subroutine for_each_matching_import
   module subroutine check_match_connection_unsatisfied(this, conn, unresolved, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MatchConnection), intent(in) :: conn
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
      type(MatchConnection), intent(in) :: conn
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
   module subroutine resolve_inout_destination(this, src_pt, dst_pt, dst_node, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), target, intent(in) :: dst_node
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

   ! openspec/changes/graph-builder-resolution-index, design.md
   ! Decisions "enumerate_qualified_exports generalizes
   ! collect_qualified_exports's own recursion": builds root's own
   ! flattened, qualified-path-indexed export enumeration by recursing
   ! across every descendant level - same traversal/path-composition
   ! shape as build_qualified_export_namespace/collect_qualified_exports
   ! (GraphBuilder_CallbackWiring_smod.F90), but returning a plain
   ! (qualified_path, owner, node_id) tuple per export (resolved via the
   ! owner's own ComponentGraph%get_resource_index(item_key(...)), once
   ! advertised) instead of QualifiedExportEntry's own
   ! var_spec-carrying shape - decoupled from callback-wiring's own
   ! record type (proposal.md "What Changes"). A fresh, uncached
   ! snapshot at resolution time, same contract as
   ! collect_qualified_exports (design.md Non-Goals).
   function enumerate_qualified_exports(root, rc) result(entries)
      class(OuterMetaComponent), target, intent(inout) :: root
      integer, optional, intent(out) :: rc
      type(QualifiedExportIndexEntry), allocatable :: entries(:)

      integer :: status

      allocate(entries(0))
      call collect_qualified_export_index(root, '', entries, _RC)

      _RETURN(_SUCCESS)
   end function enumerate_qualified_exports

   recursive subroutine collect_qualified_export_index(owner, path_prefix, entries, rc)
      class(OuterMetaComponent), target, intent(inout) :: owner
      character(*), intent(in) :: path_prefix
      type(QualifiedExportIndexEntry), allocatable, intent(inout) :: entries(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, num_children
      type(ComponentSpec), pointer :: comp_spec
      type(ComponentGraph), pointer :: owner_graph
      type(VariableSpecVectorIterator) :: iter
      type(VariableSpec), pointer :: var_spec
      character(:), allocatable :: child_name, child_path
      type(OuterMetaComponent), pointer :: child_meta
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

      num_children = owner%get_num_children()
      do i = 1, num_children
         child_name = owner%get_child_name(i, _RC)
         child_meta => owner%get_child_outer_meta(child_name, _RC)
         if (len(path_prefix) == 0) then
            child_path = child_name
         else
            child_path = path_prefix // '/' // child_name
         end if
         call collect_qualified_export_index(child_meta, child_path, entries, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine collect_qualified_export_index

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
   ! openspec/changes/phase-10a-state-materialization: optional `name`,
   ! passed through unmodified to materialize_field_extension()'s own
   ! new `name` argument - omitted by the mismatch-branch call site
   ! (unchanged, unnamed-field behavior), supplied by the new exact-
   ! match-branch call site (the export's own short_name) so graph-
   ! native state population can find it by name once inserted into a
   ! real ESMF_State.
   !
   ! openspec/changes/graph-builder-stateitem-resolution, design.md
   ! correction: export_item/import_item are the two already-resolved
   ! sides' own GraphStateItem payloads (read directly off their
   ! StateItemNodes by the caller) - no VariableSpec reference needed
   ! (proposal.md "nothing downstream should need to walk back to the
   ! originating VariableSpec"). itemType/units/typekind/ungridded_dims/
   ! vertical_stagger are all read from export_item/import_item's own
   ! declared_item_type/characteristics/ungridded_dims/vertical_stagger
   ! fields (populated once by advertise_one, task 2.2/2.3 and the
   ! design.md correction extending it).
   subroutine try_materialize_field_extension(this, export_comp_name, export_item, import_item, &
        graph, node_id, failure_reason, name, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      type(GraphStateItem), intent(in) :: export_item
      type(GraphStateItem), intent(in) :: import_item
      type(ComponentGraph), target, intent(inout) :: graph
      type(NodeId), intent(in) :: node_id
      character(:), allocatable, intent(out) :: failure_reason
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      class(OuterMetaComponent), pointer :: export_meta
      type(ESMF_Geom) :: geom
      logical :: has_geom
      class(VerticalGrid), allocatable :: vgrid
      type(VerticalStaggerLoc) :: vert_staggerloc
      type(VerticalStaggerLoc), allocatable :: declared_vertical_stagger
      type(ESMF_TypeKind_Flag) :: import_typekind
      character(:), allocatable :: import_units

      failure_reason = ''

      if (.not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(export_item%get_declared_item_type())
         _RETURN(_SUCCESS)
      end if
      if (.not. (import_item%get_declared_item_type() == MAPL_STATEITEM_FIELD)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(import_item%get_declared_item_type())
         _RETURN(_SUCCESS)
      end if
      import_units = get_item_units(import_item, _RC)
      _ASSERT(import_units /= '', 'GraphBuilder: materialization reached with no import units - internal inconsistency')

      export_meta => component_for(this, export_comp_name, _RC)

      call resolve_export_geom(export_item, export_meta, geom, has_geom, _RC)
      if (.not. has_geom) then
         ! design.md Decisions "Geom/vgrid resolution order" case (c):
         ! neither the export's own declared geom nor the owning
         ! OuterMetaComponent has a concrete geom - distinguishable from
         ! both the "unsupported item class" case above and 3c's own
         ! "unregistered characteristic fails loudly."
         failure_reason = 'unresolved_geom'
         _RETURN(_SUCCESS)
      end if

      ! Vertical grid is optional (a 2D field legitimately has none,
      ! same as FieldCreate's own optional vgrid argument) - unlike geom,
      ! its absence is not a failure. When present, a vert_staggerloc is
      ! required alongside it (FieldCreate's own contract); default to
      ! CENTER when the export's own declared vertical_stagger does not
      ! say otherwise (VariableSpec.F90's own make_VerticalGridAspect
      ! precedent).
      call resolve_export_vgrid(export_item, export_meta, vgrid)
      import_typekind = get_item_typekind(import_item, _RC)
      if (allocated(vgrid)) then
         vert_staggerloc = VERTICAL_STAGGER_CENTER
         call export_item%get_vertical_stagger(declared_vertical_stagger)
         if (allocated(declared_vertical_stagger)) vert_staggerloc = declared_vertical_stagger

         ! openspec/changes/phase-10c-typekind-characteristic, design.md
         ! D4: typekind/units are the IMPORT's own values (not the
         ! export's), matching this procedure's own pre-existing
         ! precedent - the materialized item's real payload must match
         ! what the IMPORT requires, not what the export happened to
         ! produce.
         call materialize_field_extension(graph, node_id, geom=geom, typekind=import_typekind, &
              units=import_units, ungridded_dims=export_item%get_ungridded_dims(), &
              vgrid=vgrid, vert_staggerloc=vert_staggerloc, name=name, _RC)
      else
         call materialize_field_extension(graph, node_id, geom=geom, typekind=import_typekind, &
              units=import_units, ungridded_dims=export_item%get_ungridded_dims(), name=name, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine try_materialize_field_extension

   ! openspec/changes/graph-builder-stateitem-resolution, design.md
   ! correction: recovers a GraphStateItem's own established
   ! PHYSICAL_UNITS_CHARACTERISTIC_KIND entry (populated once by
   ! advertise_one, task 2.2) as a plain string - the node-based
   ! equivalent of reading var_spec%units directly. Returns '' if the
   ! characteristic was never established (mirrors
   ! `allocated(var_spec%units)`'s own "not declared" gate - an item
   ! advertised with no explicit units never gets this characteristic
   ! kind inserted, build_characteristics' own prior behavior, moved
   ! verbatim).
   function get_item_units(item, rc) result(units)
      type(GraphStateItem), intent(in) :: item
      integer, optional, intent(out) :: rc
      character(:), allocatable :: units

      integer :: status
      class(StateItemCharacteristic), allocatable :: characteristic

      units = ''
      if (.not. item%has_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND)) then
         _RETURN(_SUCCESS)
      end if

      call item%get_characteristic(PHYSICAL_UNITS_CHARACTERISTIC_KIND, characteristic, _RC)
      select type (characteristic)
      class is (PhysicalUnitsCharacteristic)
         units = characteristic%get_units()
      class default
         _FAIL('GraphBuilder: get_item_units - characteristic is not a PhysicalUnitsCharacteristic')
      end select

      _RETURN(_SUCCESS)
   end function get_item_units

   ! Same role as get_item_units() above, for
   ! TYPE_KIND_CHARACTERISTIC_KIND - always established (advertise_one
   ! inserts it unconditionally, task 2.2, mirroring
   ! build_characteristics' own prior unconditional insert), so this
   ! never hits the "not established" case in production.
   function get_item_typekind(item, rc) result(type_kind)
      type(GraphStateItem), intent(in) :: item
      integer, optional, intent(out) :: rc
      type(ESMF_TypeKind_Flag) :: type_kind

      integer :: status
      class(StateItemCharacteristic), allocatable :: characteristic

      type_kind = ESMF_TYPEKIND_R4
      _ASSERT(item%has_characteristic(TYPE_KIND_CHARACTERISTIC_KIND), 'GraphBuilder: get_item_typekind - no TYPE_KIND_CHARACTERISTIC_KIND established')

      call item%get_characteristic(TYPE_KIND_CHARACTERISTIC_KIND, characteristic, _RC)
      select type (characteristic)
      class is (TypeKindCharacteristic)
         type_kind = label_to_typekind(characteristic%get_type_kind())
      class default
         _FAIL('GraphBuilder: get_item_typekind - characteristic is not a TypeKindCharacteristic')
      end select

      _RETURN(_SUCCESS)
   end function get_item_typekind

   ! openspec/changes/phase-11b-chain-intermediate-materialization,
   ! design.md D4: materializes every entry of intermediate_node_ids (in
   ! order) with that link's own correctly-evolved typekind/units
   ! (merged_item_for_link, D2 below) - replacing each of the four
   ! find_or_build_extension_chain call sites' own single final-node
   ! try_materialize_field_extension call. Resolves geom/vgrid once
   ! (resolve_export_geom/resolve_export_vgrid, unchanged) and reuses it
   ! for every link, since GEOM_CHARACTERISTIC_ID never appears in
   ! build_characteristics' own mismatch set (design.md Context).
   ! Stops at the first link whose materialization fails, returning the
   ! same distinguishable failure_reason try_materialize_field_extension
   ! itself already produces - callers' own existing failure-reporting is
   ! unchanged. mismatched_kinds MUST be the same array
   ! find_or_build_extension_chain was called with (the resolution order
   ! intermediate_node_ids is parallel to).
   subroutine materialize_extension_chain(this, export_comp_name, export_item, import_item, &
        mismatched_kinds, intermediate_node_ids, graph, failure_reason, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: export_comp_name
      type(GraphStateItem), intent(in) :: export_item
      type(GraphStateItem), intent(in) :: import_item
      type(StateItemCharacteristicKind), intent(in) :: mismatched_kinds(:)
      type(NodeId), intent(in) :: intermediate_node_ids(:)
      type(ComponentGraph), target, intent(inout) :: graph
      character(:), allocatable, intent(out) :: failure_reason
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(OuterMetaComponent), pointer :: export_meta
      type(ESMF_Geom) :: geom
      logical :: has_geom
      class(VerticalGrid), allocatable :: vgrid
      type(VerticalStaggerLoc) :: vert_staggerloc
      type(VerticalStaggerLoc), allocatable :: declared_vertical_stagger
      character(:), allocatable :: link_units
      type(ESMF_TypeKind_Flag) :: link_typekind
      character(:), allocatable :: import_units

      failure_reason = ''

      if (.not. (export_item%get_declared_item_type() == MAPL_STATEITEM_FIELD)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(export_item%get_declared_item_type())
         _RETURN(_SUCCESS)
      end if
      if (.not. (import_item%get_declared_item_type() == MAPL_STATEITEM_FIELD)) then
         failure_reason = 'unsupported_item_class:' // itemtype_name(import_item%get_declared_item_type())
         _RETURN(_SUCCESS)
      end if
      import_units = get_item_units(import_item, _RC)
      _ASSERT(import_units /= '', 'GraphBuilder: materialize_extension_chain reached with no import units - internal inconsistency')

      export_meta => component_for(this, export_comp_name, _RC)

      call resolve_export_geom(export_item, export_meta, geom, has_geom, _RC)
      if (.not. has_geom) then
          failure_reason = 'unresolved_geom'
         _RETURN(_SUCCESS)
      end if

      call resolve_export_vgrid(export_item, export_meta, vgrid)
      if (allocated(vgrid)) then
         vert_staggerloc = VERTICAL_STAGGER_CENTER
         call export_item%get_vertical_stagger(declared_vertical_stagger)
         if (allocated(declared_vertical_stagger)) vert_staggerloc = declared_vertical_stagger
      end if

      do i = 1, size(intermediate_node_ids)
         call merged_item_for_link(export_item, import_item, mismatched_kinds, i, link_units, link_typekind, _RC)

         if (allocated(vgrid)) then
            call materialize_field_extension(graph, intermediate_node_ids(i), geom=geom, &
                 typekind=link_typekind, units=link_units, &
                 ungridded_dims=export_item%get_ungridded_dims(), &
                 vgrid=vgrid, vert_staggerloc=vert_staggerloc, _RC)
         else
            call materialize_field_extension(graph, intermediate_node_ids(i), geom=geom, &
                 typekind=link_typekind, units=link_units, &
                 ungridded_dims=export_item%get_ungridded_dims(), _RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine materialize_extension_chain

   ! openspec/changes/phase-11b-chain-intermediate-materialization,
   ! design.md D2 (node-based, design.md correction): link i's own
   ! materialized typekind/units is the IMPORT item's own value for
   ! every StateItemCharacteristicKind in mismatched_kinds(1:i) (already
   ! "converted" by that point in the chain) and the EXPORT item's own
   ! value otherwise. At i == size(mismatched_kinds) this exactly
   ! reduces to try_materialize_field_extension's own existing
   ! final-node formula (every mismatched kind has import's value) - no
   ! behavior change for the single-characteristic case. _FAIL
   ! explicitly for any StateItemCharacteristicKind outside the three
   ! this helper knows how to translate ("unregistered characteristic
   ! fails loudly" applies to this translation step too).
   subroutine merged_item_for_link(export_item, import_item, mismatched_kinds, i, units, type_kind, rc)
      type(GraphStateItem), intent(in) :: export_item
      type(GraphStateItem), intent(in) :: import_item
      type(StateItemCharacteristicKind), intent(in) :: mismatched_kinds(:)
      integer, intent(in) :: i
      character(:), allocatable, intent(out) :: units
      type(ESMF_TypeKind_Flag), intent(out) :: type_kind
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: j
      logical :: units_converted
      logical :: typekind_converted
      character(:), allocatable :: assertion_message

      units = get_item_units(export_item, _RC)
      type_kind = get_item_typekind(export_item, _RC)
      units_converted = .false.
      typekind_converted = .false.

      do j = 1, i
         if (mismatched_kinds(j) == PHYSICAL_UNITS_CHARACTERISTIC_KIND) then
            units_converted = .true.
         else if (mismatched_kinds(j) == TYPE_KIND_CHARACTERISTIC_KIND) then
            typekind_converted = .true.
         else if (mismatched_kinds(j) == VERTICAL_COORDINATE_CHARACTERISTIC_KIND) then
            ! Vertical grid has no concrete field-materialization
            ! parameter today (materialize_field_extension's own vgrid/
            ! vert_staggerloc are resolved once from export_item, outside
            ! this per-link loop, by materialize_extension_chain itself -
            ! unaffected by this helper). Nothing to merge here;
            ! VerticalCoordinateCharacteristic%build_transform always
            ! fails, so this branch is unreachable in practice, but is
            ! enumerated explicitly rather than falling into the _FAIL
            ! default below.
         else
            assertion_message = 'GraphBuilder: merged_item_for_link - unrecognized StateItemCharacteristicKind: ' // &
                 mismatched_kinds(j)%to_string()
            _FAIL(assertion_message)
         end if
      end do

      if (units_converted) units = get_item_units(import_item, _RC)
      if (typekind_converted) type_kind = get_item_typekind(import_item, _RC)

      _RETURN(_SUCCESS)
   end subroutine merged_item_for_link

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

   ! Resolves a (component, intent, short_name) reference to the item's
   ! own real NodeId, wherever it actually lives in the hierarchy:
   !  - "<self>"/this's own name: the item's own NodeId, in THIS
   !    component's own graph (must already have been advertised -
   !    graphbuilder_advertise() runs first).
   !  - a named descendant (direct child or any further-qualified
   !    `/`-separated path): the descendant's own real NodeId, read
   !    directly from that descendant's own ComponentGraph via
   !    resolve_qualified_component's existing one-directional tree walk
   !    (never resolves upward past `this`, per graph/component-
   !    hierarchy's "Connection endpoint resolution never resolves
   !    upward past its declaring component" requirement - the walk
   !    itself has no "go to my parent" code path to begin with, so this
   !    is structural, not merely checked).
   !
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 3.1/3.2:
   ! replaces get_or_make_local_node_id's own proxy-creation branch -
   ! with node/network storage shared hierarchy-wide (graph/component-
   ! hierarchy's "One shared hierarchy-wide graph store" requirement), a
   ! descendant's own real NodeId is already a valid edge endpoint from
   ! any calling component's own ComponentGraph%add_dependency() (see
   ! that method's own header comment in ComponentGraph.F90); no
   ! parent-local stand-in node is created, cached, or returned.
   function resolve_local_node_id(this, comp_name, state_intent, short_name, rc) result(node_id)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: comp_name
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc
      type(NodeId) :: node_id

      integer :: status
      character(:), allocatable :: item_id_key
      type(ComponentGraph), pointer :: owner_graph
      type(NodeId), pointer :: existing
      type(OuterMetaComponent), pointer :: owner_meta

      item_id_key = item_key(state_intent, short_name)

      owner_meta => resolve_qualified_component(this, comp_name, _RC)
      owner_graph => owner_meta%get_component_graph()
      existing => owner_graph%get_resource_index(item_id_key)
      _ASSERT(associated(existing), 'GraphBuilder: connection references an item "' // comp_name //
           '" never advertised: ' // short_name)
      node_id = existing

      _RETURN(_SUCCESS)
   end function resolve_local_node_id

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

   ! Orchestrates one callback-declaring destination's full resolution:
   ! wildcard match + interface conformance (resolve_callback_import
   ! above), materialization (materialize_callback_collection above),
   ! wiring to the already-advertised destination import via an ordinary
   ! add_dependency edge (design.md Decision 5 - the same "producer node
   ! feeds a consumer import" shape ordinary/extension-chain resolution
   ! already uses), and per-match CallbackStateBinding construction
   ! (build_callback_state_binding, below). Idempotent: a resource-index
   ! key derived from the destination import's own NodeId is checked
   ! before rebuilding (mirrors find_or_build_extension_chain's own
   ! chain_key reuse idiom) - a repeat resolve_connections() call is
   ! then a no-op (spec scenario "Re-resolving the same connection does
   ! not duplicate collection members").
   !
   ! openspec/changes/graph-builder-stateitem-resolution: var_spec
   ! replaced by dst_short_name/expected_interface_id (plain arguments) -
   ! this procedure only ever read %short_name/%callback_interface_id
   ! off it.
   subroutine resolve_callback_destination(this, src_pt, dst_pt, dst_short_name, expected_interface_id, rejected, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      character(*), intent(in) :: dst_short_name
      type(CallbackInterfaceId), intent(in) :: expected_interface_id
      type(StringVector), optional, intent(out) :: rejected
      integer, optional, intent(out) :: rc

      integer :: status
      type(ComponentGraph), pointer :: graph
      type(NodeId) :: import_id
      character(:), allocatable :: collection_key
      type(NodeId), pointer :: existing_collection
      type(QualifiedExportEntry), allocatable :: matches(:)
      type(NodeId) :: collection_id
      type(DependencyNetworkId) :: net_id

      graph => this%get_component_graph()

      import_id = resolve_local_node_id(this, dst_pt%component_name, ESMF_STATEINTENT_IMPORT, dst_short_name, _RC)

      collection_key = 'CALLBACK_COLLECTION:' // import_id%to_string()
      existing_collection => graph%get_resource_index(collection_key)
      if (associated(existing_collection)) then
         if (present(rejected)) rejected = StringVector()
         _RETURN(_SUCCESS)
      end if

      matches = resolve_callback_import(this, src_pt, expected_interface_id, rejected, _RC)

      collection_id = materialize_callback_collection(this, src_pt, matches, _RC)
      call graph%add_resource_index(collection_key, collection_id, _RC)

      net_id = graph%get_default_network_id()
      call graph%add_dependency(net_id, collection_id, import_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine resolve_callback_destination


end module mapl_GraphBuilder_mod
