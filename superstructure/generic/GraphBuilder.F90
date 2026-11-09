#include "MAPL.h"

!------------------------------------------------------------------------------
! GraphBuilder: integration layer between the graph-neutral
! ComponentGraph/DependencyNetwork core and the rest of MAPL
! It is the only layer allowed to depend on both the
! graph core and OuterMetaComponent.
!
! Scope: advertising (StateItemNodes for import/export/internal items),
! connection resolution for MatchConnection (the Connection subtype behind
! OuterMetaComponent%connect_all), extension chains for mismatched pairs
! (mapl_ExtensionResolution_mod), validate/freeze, geometry, state
! population, callback wiring, and run-phase nodes. Not in scope:
! SimpleConnection/ReexportConnection and compiled execution.
!
! Stateless per call: every procedure takes the OuterMetaComponent it
! operates on and reads/writes only that component's ComponentGraph (via
! get_component_graph), plus a named child's graph/spec via
! get_child_component_graph()/get_child_component_spec(). That reach is a
! framework-internal carve-out, not part of
! OuterMetaComponent's public API.
!
! Advertised-item lookup uses ComponentGraph's semantic resource index
! keyed by item_key().
!
! Cross-component connections: node storage is shared hierarchy-wide, so
! a dependency edge references a child's own real NodeId directly; no
! parent-local proxy nodes exist. The edge lives in the network of the
! component that declared the connection.
!
! Two-phase timing mirrors legacy Connection activate()/connect():
!   - graphbuilder_check_unsatisfied_imports(): activate()-time analog;
!     read-only, creates no graph structure. Called via run_activate_hook
!     from initialize_advertise.F90.
!   - graphbuilder_resolve_connections(): connect()-time analog; the real
!     graph mutation (dependency edges, extension chains). Called via
!     run_connect_hook from initialize_accept_transfer.F90.
! Freezing is a separate, later phase (graphbuilder_run_graph_freeze_hook).
!
! Horizontal geometry: not a VariableSpec. GeometrySpec already decides a
! component's geometry (initialize_geom_a/b.F90). GEOM_A/GEOM_B run after
! ADVERTISE, so the geometry hook is called from initialize_geom_b.F90,
! bottom-up, once every component's geom is real.
! graphbuilder_run_geometry_hook only gives that decision graph structure:
!   - graphbuilder_advertise_geometry(): wraps this%get_geom() as a
!     geometry-proxy GraphStateItem registered as a StateItemNode under
!     the reserved GEOMETRY_ITEM_NAME key. It is deliberately not in
!     ComponentSpec%var_specs, so it cannot reach add_to_states or a
!     user-facing state.
!   - graphbuilder_resolve_geometry(): adds a dependency edge for the
!     resolved parent/child relationship, in the pull (from child) and
!     push (to child) directions. A mismatch would mean GeometrySpec and
!     the graph disagree; it is asserted, not trusted.
! The whole geometry path is a no-op when graph_mode() is
! GRAPH_MODE_REGISTRY.
!------------------------------------------------------------------------------
module mapl_GraphBuilder_mod
   use esmf, only: ESMF_Field, ESMF_Geom, ESMF_STATEINTENT_EXPORT, ESMF_StateIntent_Flag, ESMF_STATEINTENT_IMPORT, ESMF_StateItem_Flag, operator(==)
   use gFTL2_StringVector, only: StringVector
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId
   use mapl_CallbackInterfaceRegistry_mod, only: get_callback_interface
   use mapl_CallbackMethodBinding_mod, only: CallbackMethodBinding
   use mapl_CallbackStateBinding_mod, only: CallbackStateBinding
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap
   use mapl_StateItemCharacteristicKind_mod, only: &
        StateItemCharacteristicKind, operator(==)
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_ComponentSpec_mod, only: ComponentSpec
   use mapl_Connection_mod, only: Connection
   use mapl_ConnectionPt_mod, only: ConnectionPt
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_ExtensionMaterialization_mod, only: materialize_field_extension
   use mapl_GraphResolutionEntryVector_mod, only: GraphResolutionEntryVector
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_StateItemDescriptor_mod, only: StateItemDescriptor
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
   use mapl_VirtualConnectionPt_mod, only: VirtualConnectionPt
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: GraphBuilder
   public :: item_key
   ! Public because gfortran gives an ordinary (non-`module`) procedure in
   ! a private-by-default module internal linkage, so a separately
   ! compiled submodule cannot link to it (NAG does not have this
   ! limitation). Each is called from at least one submodule.
   public :: component_for
   public :: is_self
   public :: itemtype_name
   ! Public so tests and other code that identify a geometry node need not
   ! duplicate the key format.
   public :: GEOMETRY_ITEM_NAME

   ! Public so tests can exercise callback resolution steps directly,
   ! not only through GraphBuilder%resolve_connections() and
   ! check_unsatisfied_imports().
   public :: QualifiedExportEntry
   public :: build_qualified_export_namespace
   public :: resolve_callback_import
   public :: materialize_callback_collection
   public :: build_callback_state_binding
   public :: build_callback_method_binding
   public :: invoke_callback_method

   ! Shared multi-level qualified-path resolution, used by callback and
   ! ordinary connection resolution. Public for the same gfortran
   ! cross-submodule-linkage reason as component_for/is_self above.
   public :: resolve_qualified_component
   public :: QualifiedExportIndexEntry
   ! History support: given a qualified component path, returns that
   ! component's own resource-index entries (no recursion into children,
   ! no push/propagation). Composes resolve_qualified_component with the
   ! same per-item scan as append_own_export_index_entries.
   public :: query_component_exports

   ! Same gfortran cross-submodule-linkage reason as component_for/
   ! is_self above: called from GraphBuilder_LifecycleHooks_smod.F90,
   ! defined in GraphBuilder_ConnectionResolution_smod.F90.
   public :: UnresolvedImportEntry
   public :: graphbuilder_unresolved_import_node_ids
   public :: graphbuilder_run_propagate_hook
   public :: graphbuilder_run_export_propagate_hook
   public :: graphbuilder_run_reexport_hook
   public :: graphbuilder_allocate_internals

   ! Stateless by design: every bound procedure still takes the
   ! OuterMetaComponent it operates on. All bindings are NOPASS, e.g.
   ! `gb = GraphBuilder(); call gb%run_advertise_hook(this)`.
   type :: GraphBuilder
   contains
      procedure, nopass :: advertise => graphbuilder_advertise
      procedure, nopass :: check_unsatisfied_imports => graphbuilder_check_unsatisfied_imports
      procedure, nopass :: resolve_connections => graphbuilder_resolve_connections
      procedure, nopass :: freeze => graphbuilder_freeze
      procedure, nopass :: build_label_map => graphbuilder_build_label_map
      procedure, nopass :: run_advertise_hook => graphbuilder_run_advertise_hook
      ! For each direct child, mirrors that child's still-unresolved
      ! imports into THIS component's import-port table under the
      ! child's own real NodeId (no search, no proxy, no composite
      ! wrapper). Called from initialize_advertise.F90 right after
      ! recurse() returns and before this component's run_activate_hook.
      procedure, nopass :: run_propagate_hook => graphbuilder_run_propagate_hook
      ! Export propagation, symmetric to run_propagate_hook but
      ! unconditional (no "already resolved" gate), matching legacy
      ! propagate_exports_subregistry. Called from initialize_advertise.F90
      ! where legacy calls registry%propagate_exports.
      procedure, nopass :: run_export_propagate_hook => graphbuilder_run_export_propagate_hook
      ! Gives each ReexportConnection of `this` an export port under the
      ! destination name, bound to the source's existing node. Called from
      ! initialize_advertise.F90 after run_advertise_hook and before the
      ! export propagation of this component's parent reads these ports.
      procedure, nopass :: run_reexport_hook => graphbuilder_run_reexport_hook
      ! Completes and allocates this component's INTERNAL Fields at
      ! REALIZE_ACCEPTED (where legacy allocates them).
      procedure, nopass :: allocate_internals => graphbuilder_allocate_internals
      procedure, nopass :: run_activate_hook => graphbuilder_run_activate_hook
      procedure, nopass :: run_connect_hook => graphbuilder_run_connect_hook
      ! Resolves an EXPRESSION item's named variable references against
      ! already-advertised sibling exports and wires in its
      ! ExpressionTransform's TransformGraphNode. Idempotent
      ! (resource-index guard). Separate from resolve_one's pairwise loop
      ! and from ExtensionResolution's build_chain. Called from
      ! initialize_accept_transfer.F90 right after run_connect_hook, so
      ! every sibling export is already advertised.
      procedure, nopass :: run_expression_hook => graphbuilder_run_expression_hook
      ! GENERIC_INIT_GRAPH_FREEZE entry point
      ! (OuterMetaComponent/initialize_graph_freeze.F90), called once on
      ! the hierarchy root only. Freezes every component's graph in one
      ! tree walk, then fails if any required import remains unresolved
      ! anywhere in the hierarchy.
      procedure, nopass :: run_graph_freeze_hook => graphbuilder_run_graph_freeze_hook
      ! In strict validation mode, fails if any pair in the subtree was left
      ! unwired as UNSUPPORTED; otherwise a no-op. Joins
      ! run_graph_freeze_hook once legacy no longer connects such pairs.
      procedure, nopass :: check_unsupported_pairs => graphbuilder_check_unsupported_pairs
      ! No-op when graph_mode() is GRAPH_MODE_REGISTRY.
      procedure, nopass :: run_geometry_hook => graphbuilder_run_geometry_hook
      ! Fills DEFERRED item characteristics from the component's default
      ! geom (GEOM_B) or from the geom the component put on the item
      ! (harvest, called at connect time).
      procedure, nopass :: resolve_deferred => graphbuilder_resolve_deferred
      procedure, nopass :: harvest_deferred => graphbuilder_harvest_deferred
      ! Graph-native equivalent of StateRegistry%add_to_states: inserts
      ! each resolved Field item into a real ESMF_State under its short
      ! name. A real producer only in GRAPH_MODE_GRAPH. rc-returning for
      ! direct tests; run_populate_state_hook is the real
      ! GENERIC_INIT_ACCEPT_TRANSFER entry.
      procedure, nopass :: populate_state => graphbuilder_populate_state
      procedure, nopass :: run_populate_state_hook => graphbuilder_run_populate_state_hook
      ! Fills the import/export States the PARENT sees (legacy
      ! add_to_states mode='outer') with the same Fields as the user
      ! states. GRAPH_MODE_GRAPH with graph_populates_user_states() only.
      procedure, nopass :: populate_outer_state => graphbuilder_populate_outer_state
      procedure, nopass :: run_populate_outer_hook => graphbuilder_run_populate_outer_hook
      ! Construction only; see graphbuilder_build_run_phase_node. Do not
      ! add an invocation hook without resolving the blockers named there.
      procedure, nopass :: build_run_phase_node => graphbuilder_build_run_phase_node
      ! Pulls the component's own imports through the forward network
      ! before its run (ComponentGraph%update() only, never a
      ! MethodGraphNode or a second driver%run()), and advances its own
      ! exports after a successful run. Called from run_user.F90. No-op
      ! outside GRAPH_MODE_GRAPH.
      procedure, nopass :: run_pull_hook => graphbuilder_run_pull_hook
      procedure, nopass :: run_advance_hook => graphbuilder_run_advance_hook
   end type GraphBuilder

   ! Matches StateRegistry_Hierarchy_smod's own SELF sentinel
   ! (superstructure/generic/registry/StateRegistry_Hierarchy_smod.F90):
   ! a ConnectionPt component_name of "<self>" always refers to the
   ! component the connection was declared on.
   character(*), parameter :: SELF_COMPONENT_NAME = '<self>'

   ! Reserved item name for a component's resolved horizontal geometry,
   ! indexed with item_key() like ordinary items but never inserted into
   ! ComponentSpec%var_specs, so it cannot reach add_to_states or a
   ! user-facing state.
   character(*), parameter :: GEOMETRY_ITEM_NAME = 'MAPL_Geometry'

   ! Per-match callback for for_each_matching_import(). Callers pass an
   ! internal procedure so it can host-associate its context (this,
   ! src_pt, unresolved, ...). The argument is a StateItemNode because
   ! for_each_matching_import iterates the destination graph's import
   ! ports, not var_specs.
   abstract interface
      subroutine I_match_op(node, rc)
         import StateItemNode
         class(StateItemNode), target, intent(in) :: node
         integer, optional, intent(out) :: rc
      end subroutine I_match_op
   end interface

   ! One entry in the flattened qualified-export namespace
   ! (build_qualified_export_namespace). comp_path is '' for the
   ! namespace root's own export, else the '/'-joined descendant path
   ! (e.g. 'CHEM/DU'). Public, not opaque: matching and interface
   ! conformance need the VariableSpec, and resolving a match to a NodeId
   ! needs comp_path.
   type :: QualifiedExportEntry
      character(:), allocatable :: qualified_name
      character(:), allocatable :: comp_path
      type(VariableSpec) :: var_spec
   end type QualifiedExportEntry

   ! One (qualified_path, owner, node_id) tuple from a resolved-export
   ! scan. Deliberately carries no VariableSpec, unlike
   ! QualifiedExportEntry. qualified_path is '' for the enumeration
   ! root's own export, else the '/'-joined descendant path.
   type :: QualifiedExportIndexEntry
      character(:), allocatable :: qualified_path
      type(OuterMetaComponent), pointer :: owner => null()
      type(NodeId) :: node_id
   end type QualifiedExportIndexEntry

   ! One entry in graphbuilder_unresolved_import_node_ids's scan result:
   ! a still-unresolved import, by short_name (for GraphResolutionEntry
   ! reporting) and its already-real node_id (what
   ! graphbuilder_run_propagate_hook mirrors, no copy).
   type :: UnresolvedImportEntry
      character(:), allocatable :: short_name
      type(NodeId) :: node_id
   end type UnresolvedImportEntry

   ! Matches mapl_MethodInvocation_mod's MAX_ARGUMENT_NAME_LEN.
   integer, parameter :: MAX_CALLBACK_ARGUMENT_NAME_LEN = 128

   ! ---- Interface block: one declaration per procedure whose body is in
   ! a submodule ----

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
   ! Imports in `child_meta`'s import-port table that are still unresolved
   ! at ADVERTISE time. "Resolved" cannot be read from dependency edges
   ! (none exist yet; they are added at ACCEPT_TRANSFER), nor from
   ! graphbuilder_check_unsatisfied_imports on the child alone (it only
   ! sees connections the child itself declares, missing a parent's
   ! connection naming the child, and mirroring an already-resolved
   ! import risks a two-producer violation in the forward network). An import is
   ! unresolved iff NEITHER (a) a MatchConnection declared on `child_meta`
   ! whose destination resolves back to it, NOR (b) a MatchConnection
   ! declared on `this_parent` whose destination resolves to `child_meta`,
   ! finds a matching source export (the per-item check of
   ! check_match_connection_unsatisfied, used as a positive match).
   ! `child_meta` MUST be a direct child of `this_parent`. Read-only.
   module function graphbuilder_unresolved_import_node_ids(this_parent, child_meta, rc) result(unresolved)
      class(OuterMetaComponent), target, intent(inout) :: this_parent
      class(OuterMetaComponent), target, intent(inout) :: child_meta
      integer, optional, intent(out) :: rc
      type(UnresolvedImportEntry), allocatable :: unresolved(:)

   end function graphbuilder_unresolved_import_node_ids
   ! For each direct child C of `this` (unconditionally), mirrors C's
   ! still-unresolved imports (graphbuilder_unresolved_import_node_ids)
   ! into `this`'s import-port table under C's own real NodeId; no new
   ! node. Called from initialize_advertise.F90 right after
   ! run_activate_hook, not before: like legacy's
   ! propagate_unsatisfied_imports, it needs the child's activate-time
   ! resolution done first, or already-resolved imports would be
   ! mirrored upward needlessly.
    module subroutine graphbuilder_run_propagate_hook(this)
       class(OuterMetaComponent), target, intent(inout) :: this

    end subroutine graphbuilder_run_propagate_hook
    ! For each direct child C of `this` (unconditionally), propagates C's
    ! export-port entries into `this`'s export-port table under C's own
    ! real NodeId, like run_propagate_hook. Unlike imports, the entry's
    ! effective name is qualified with C's name (C_name/short_name),
    ! following legacy propagate_exports_virtual_pt: an already-qualified
    ! name passes through unchanged at higher levels (export E of A is A/E
    ! in the parent and still A/E in the grandparent). The qualified name
    ! is never written to the node's short_name (that belongs to the
    ! advertising component); it lives only in the export-port table
    ! (ComponentGraph%add_export_port qualified_name), which
    ! find_export_node matches against.
    module subroutine graphbuilder_run_export_propagate_hook(this)
       class(OuterMetaComponent), target, intent(inout) :: this

    end subroutine graphbuilder_run_export_propagate_hook

    ! For each ReexportConnection of `this`, adds an export port named by the
    ! destination, bound to the source's existing NodeId (no new node; an
    ! INTERNAL source stays INTERNAL, only its visibility changes). The source
    ! is an own internal (found by resource index) or an exported item of any
    ! descendant (found by export-port name, so a propagated name resolves).
    ! Fails if the source does not exist or the destination name is already
    ! taken by a different node. Unlike its neighbours this hook propagates
    ! failure: a re-export that cannot resolve must not be silent.
    module subroutine graphbuilder_run_reexport_hook(this, rc)
       class(OuterMetaComponent), target, intent(inout) :: this
       integer, optional, intent(out) :: rc
    end subroutine graphbuilder_run_reexport_hook
   module subroutine graphbuilder_allocate_internals(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
   end subroutine graphbuilder_allocate_internals
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
         unsupported_characteristics, connection_conflicts, connection_warnings, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved_imports
      ! Matched pairs whose mismatch has no registered extension provider
      ! (mapl_ExtensionResolution_mod); distinct from unresolved_imports,
      ! which means "no export at all".
      type(GraphResolutionEntryVector), optional, intent(out) :: unsupported_characteristics
      ! Matched pairs that can never connect (a characteristic's build_transform
      ! reports them incompatible); the entry reason names the kind and both values.
      type(GraphResolutionEntryVector), optional, intent(out) :: connection_conflicts
      ! Wired pairs with a tolerated mismatch (StateItemCharacteristic%match_warning).
      type(GraphResolutionEntryVector), optional, intent(out) :: connection_warnings
      integer, optional, intent(out) :: rc

    end subroutine graphbuilder_resolve_connections
   module subroutine resolve_match_connection(this, conn, unresolved, unsupported, conflicts, warnings, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      type(GraphResolutionEntryVector), intent(inout) :: unresolved
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      type(GraphResolutionEntryVector), intent(inout) :: conflicts
      type(GraphResolutionEntryVector), intent(inout) :: warnings
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
   module subroutine resolve_inout_destination(this, src_pt, dst_pt, dst_node, effective_src_name, unsupported, &
        conflicts, warnings, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ConnectionPt), intent(in) :: src_pt
      type(ConnectionPt), intent(in) :: dst_pt
      class(StateItemNode), target, intent(in) :: dst_node
      character(*), intent(in) :: effective_src_name
      type(GraphResolutionEntryVector), intent(inout) :: unsupported
      type(GraphResolutionEntryVector), intent(inout) :: conflicts
      type(GraphResolutionEntryVector), intent(inout) :: warnings
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

      ! Horizontal geometry procedures (GraphBuilder_Geometry_smod.F90)
   module subroutine graphbuilder_run_geometry_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

   end subroutine graphbuilder_run_geometry_hook
   ! Implemented in GraphBuilder_ConnectionResolution_smod.F90; declared
   ! here only to sit with the other per-hook interfaces. unresolved
   ! (optional) reports an expression's referenced name with no matching
   ! sibling export; unsupported reports nested, self, and empty expressions.
   module subroutine graphbuilder_run_expression_hook(this, unusable, &
        unresolved, unsupported, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      type(GraphResolutionEntryVector), optional, intent(out) :: unresolved
      type(GraphResolutionEntryVector), optional, intent(out) :: unsupported
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_run_expression_hook
   module subroutine graphbuilder_resolve_deferred(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_resolve_deferred
   module recursive subroutine graphbuilder_harvest_deferred(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_harvest_deferred
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
   module subroutine graphbuilder_run_graph_freeze_hook(this)
      class(OuterMetaComponent), target, intent(inout) :: this

   end subroutine graphbuilder_run_graph_freeze_hook
   module subroutine graphbuilder_check_unsupported_pairs(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
   end subroutine graphbuilder_check_unsupported_pairs

      ! Graph-native state population procedures (GraphBuilder_StatePopulation_smod.F90)
   module subroutine graphbuilder_populate_state(this, multi_state, rc, &
        own_only)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      integer, optional, intent(out) :: rc
      logical, optional, intent(in) :: own_only

   end subroutine graphbuilder_populate_state
   module subroutine graphbuilder_populate_outer_state(this, multi_state, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_populate_outer_state
   module subroutine populate_one(graph, net, multi_state, id_ptr, &
        state_intent, short_name, own_only, may_fallback, rc)
      type(ComponentGraph), target, intent(in) :: graph
      type(DependencyNetwork), intent(in) :: net
      type(MultiState), intent(inout) :: multi_state
      type(NodeId), pointer, intent(in) :: id_ptr
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      logical, intent(in) :: own_only
      logical, intent(in) :: may_fallback
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
   module subroutine graphbuilder_run_advance_hook(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_run_advance_hook

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
   module subroutine pull_callback_get_arguments(graph, get_bindings, rc)
      class(ComponentGraph), target, intent(in) :: graph
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
   module subroutine graphbuilder_run_connect_hook(this, fallback_warnings)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(GraphResolutionEntryVector), optional, intent(out) :: &
           fallback_warnings

   end subroutine graphbuilder_run_connect_hook
   module subroutine graphbuilder_fallback_unsupplied_deferred(this, &
        warnings, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(GraphResolutionEntryVector), intent(inout) :: warnings
      integer, optional, intent(out) :: rc

   end subroutine graphbuilder_fallback_unsupplied_deferred
   module subroutine graphbuilder_run_populate_state_hook(this, &
        multi_state, own_only)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      logical, optional, intent(in) :: own_only

   end subroutine graphbuilder_run_populate_state_hook
   module subroutine graphbuilder_run_populate_outer_hook(this, multi_state)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state

   end subroutine graphbuilder_run_populate_outer_hook
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
   ! Each is called from more than one GraphBuilder_*_smod.F90, so it is
   ! CONTAINS-ed here and seen by every submodule via host association.

contains

   ! Advertised-item identity key: state intent + short name, unique
   ! within one component. Also the per-item lookup key when a component's
   ! items are read from a different component's ComponentGraph.
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

   ! Returns the OuterMetaComponent named by comp_name: `this` if
   ! is_self, else the direct child via get_child_outer_meta (the
   ! framework-internal carve-out, not OuterMetaComponent's public API).
   ! Returns the object, not its ComponentSpec, because the
   ! component-wide geom/vertical_grid defaults live on
   ! OuterMetaComponent.
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

   ! Walks path's '/'-separated segments via get_child_outer_meta, one
   ! hop per segment, from root to the component the last segment names;
   ! component_for generalized to arbitrary depth. The first segment is
   ! checked against is_self(root, segment) first, so a single-segment
   ! path equal to root's name (or "<self>") resolves to root rather than
   ! a child lookup. path MUST be non-empty: root itself is reached by
   ! that is_self short-circuit, not by an empty path.
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

   ! Per-item resolved-export scan, factored out so query_component_exports
   ! can reuse it for ONE component without recursing into children.
   ! Appends owner's resolved exports (one entry per export present in
   ! its resource index; an unadvertised or unresolved export adds none)
   ! to entries, in place.
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

            ! Classic ifort ICE on array-constructor growth; see
            ! collect_qualified_exports.
            n = size(entries)
            allocate(grown_entries(n + 1))
            if (n > 0) grown_entries(1:n) = entries
            grown_entries(n + 1) = entry
            call move_alloc(grown_entries, entries)
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine append_own_export_index_entries

   ! Resolves `path` against hierarchy root `root` (same convention as
   ! resolve_qualified_component, including resolving to root itself) and
   ! returns ONLY that component's own resource-index entries: no
   ! recursion, no push/propagation. A hierarchy-wide consumer (e.g.
   ! History) walks the OuterMetaComponent tree itself, calling this per
   ! path.
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

   ! Human-readable item-class name for unsupported-item-class failure
   ! messages; mirrors VariableSpec.F90's %itemType%ot select-case.
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

   ! Expands the connection's source pattern (src_pt%v_pt)
   ! against the flattened qualified-export namespace rooted at
   ! src_pt%component_name, keeping only matches that implement the
   ! CallbackInterface for expected_interface_id. rejected (optional)
   ! collects a reason string per rejected match. Read-only (never
   ! mutates a ComponentGraph), so safe from both the activate-time check
   ! and connect-time resolution.
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

         ! Classic ifort ICE on `matches = [matches, namespace(i)]`; see
         ! collect_qualified_exports.
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
