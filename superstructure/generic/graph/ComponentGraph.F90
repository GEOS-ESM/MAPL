#include "MAPL.h"

!------------------------------------------------------------------------------
! ComponentGraph: graph-neutral owner of a component's own public import/
! export ports, child-port bindings, transform port-binding table, semantic
! resource index, port-identity generation, and mutable/frozen/finalized
! lifecycle state (spec/07-component-graph.md REQ-CG-001).
!
! openspec/changes/hierarchy-wide-component-graph: the polymorphic node map
! (NodeId -> GraphNode) and dependency-network map (DependencyNetworkId ->
! DependencyNetwork), together with the node/network identity generators,
! are NOT owned here any longer - they live in one HierarchyGraph instance
! per hierarchy, held in the private HierarchyId-keyed registry
! (mapl_HierarchyRegistry_mod). This type holds only a HierarchyId (a
! plain value, not a pointer) and resolves the shared storage via
! get_hierarchy_resources(this%hierarchy_id) wherever it is needed,
! rather than caching any reference to it as a stored component -
! design.md D1. Each instance additionally keeps its own explicit,
! private NodeIdSet/DependencyNetworkIdSet recording which ids it itself
! registered (design.md D2) - "owned" now means "recorded in this
! instance's own explicit set," not "present in a shared map," since
! every component's ids live together in the one shared HierarchyGraph.
!
! MUST NOT depend on OuterComponent, StateRegistry, GriddedComponentDriver,
! GraphBuilder, or any component-hierarchy type (REQ-CG-002) - this module
! only uses NodeId/DependencyNetworkId/PortId/HierarchyId, the GraphNode
! hierarchy, and DependencyNetwork, all of which are already graph-neutral.
!
! Public import/export/child-port storage is a bare PortId/String -> NodeId
! lookup with lifecycle-aware add/get. Named-argument transform-port
! semantics (REQ-XFORM-004/005) are a separate concern, added on top: port
! *declarations* live on TransformGraphNode itself (graph-neutral, no
! ComponentGraph involvement); ComponentGraph additionally owns the
! external port *binding* table (bind_port()/get_port_binding()/
! get_port_bindings(), keyed by (DependencyNetworkId, NodeId) -> port name
! -> NodeId, mapl_PortBindingTable_mod) - REQ-XFORM-005 settles this as external
! storage, never on-node.
!
! update() (spec/11-revision-and-update.md REQ-REV-005..007/009) is a
! type-bound procedure here rather than a free subroutine in its own
! module: it is, in every observable sense, "a ComponentGraph operation"
! (exactly like validate()/freeze()), and giving it that home matches
! this type's existing API style rather than introducing an external
! module whose only reason to exist would be file-size management - see
! ComponentGraph_DemandDrivenUpdate.F90 (a genuine Fortran submodule) for
! why file size is handled that way instead. This is a deliberate
! contrast with mapl_GraphExport_mod (spec/19-visualization-export.md
! REQ-VIZ-003), which stays a genuinely separate module on purpose: a
! submodule has full access to this type's private components (the
! standard does not treat a submodule as an additional encapsulation
! boundary), so folding the exporter in the same way would silently
! remove the "depends only on the public query API" guarantee REQ-VIZ-003
! asks for - update() has no such external requirement to satisfy.
!
! Port identities (PortId) are the only remaining per-instance generator
! (ports were never hierarchy-wide); the three PortId -> NodeId lookup
! tables (REQ-CG-001: public import ports, public export ports, child-port
! bindings) are bundled into GraphPortRegistry, a private, module-internal
! aggregation type (not part of the public API; ownership is unaffected
! since it is still transitively owned by ComponentGraph). The two
! independent frozen/finalized booleans are likewise collapsed into a
! single lifecycle_status, since a ComponentGraph only ever occupies one
! of three mutually exclusive states (mutable/frozen/finalized) - see
! graph_freeze() and graph_finalize() below for the only two transitions.
!
! Iteration convention: every gFTL walk here uses ftn_begin()/ftn_end() with
! the %next() call at the top of the loop body (iter = c%ftn_begin() sits
! one position before the first element; next() must run before the first
! use of iter).
!------------------------------------------------------------------------------
module mapl_ComponentGraph_mod
   use mapl_NodeId_mod
   use mapl_DependencyNetworkId_mod
   use mapl_PortId_mod
   use mapl_HierarchyId_mod, only: HierarchyId
   use mapl_HierarchyResources_mod, only: HierarchyResources
   use mapl_HierarchyRegistry_mod, only: request_new_hierarchy, get_hierarchy_resources
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_OperationGraphNode_mod, only: OperationGraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_TransformGraphNode_mod, only: TransformGraphNode
   use mapl_PortSpec_mod, only: PortSpec
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_NodeIdSet_mod
   use mapl_DependencyNetworkIdSet_mod, only: DependencyNetworkIdSet, DependencyNetworkIdSetIterator, operator(/=)
   use mapl_PortIdNodeIdMap_mod, only: PortIdNodeIdMap
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_PortBindingTable_mod, only: PortBindingTable
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, operator(==)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ComponentGraph

   integer, parameter :: GRAPH_LIFECYCLE_MUTABLE = 1
   integer, parameter :: GRAPH_LIFECYCLE_FROZEN = 2
   integer, parameter :: GRAPH_LIFECYCLE_FINALIZED = 3

   ! Private aggregation of the three PortId -> NodeId lookup tables
   ! (REQ-CG-001: public import ports, public export ports, child-port
   ! bindings) - module-internal, not part of the public API.
   type :: GraphPortRegistry
      private
      type(PortIdNodeIdMap) :: import_ports
      type(PortIdNodeIdMap) :: export_ports
      type(PortIdNodeIdMap) :: child_bindings
   end type GraphPortRegistry

   type :: ComponentGraph
      private
      type(HierarchyId) :: hierarchy_id
      ! design.md D2: explicit, per-instance "which ids are mine" record -
      ! a forward record, never a reverse hierarchy-wide lookup. Populated
      ! at the same point a node/network is registered (graph_register_node/
      ! graph_create_network below); consulted by owns_node()/owns_network()
      ! and by validate()/add_dependency() in place of the per-instance map
      ! membership this type used to have before shared storage.
      type(NodeIdSet) :: own_node_ids
      type(DependencyNetworkIdSet) :: own_network_ids
      type(PortIdGenerator) :: port_generator
      type(DependencyNetworkId) :: default_network_id
      type(GraphPortRegistry) :: ports
      type(PortBindingTable) :: transform_port_bindings
      type(StateItemMemberMap) :: resource_index
      integer :: lifecycle_status = GRAPH_LIFECYCLE_MUTABLE
   contains
      ! -- node registry --------------------------------------------------
      procedure :: next_node_id => graph_next_node_id
      procedure :: register_node => graph_register_node
      procedure :: get_node => graph_get_node
      procedure :: owns_node => graph_owns_node
      ! openspec/changes/hierarchy-wide-component-graph, tasks.md 3.2:
      ! "registered somewhere in this hierarchy, not necessarily by this
      ! instance" - see graph_add_dependency's own header comment for
      ! why this is distinct from, and strictly narrower than, "anything
      ! goes."
      procedure :: allowed_foreign_node => graph_allowed_foreign_node
      ! -- network registry ------------------------------------------------
      procedure :: get_default_network_id => graph_get_default_network_id
      procedure :: create_network => graph_create_network
      procedure :: get_network => graph_get_network
      procedure :: owns_network => graph_owns_network
      procedure :: add_dependency => graph_add_dependency
      ! -- ports / bindings / resource index --------------------------------
      procedure :: next_port_id => graph_next_port_id
      procedure :: add_import_port => graph_add_import_port
      procedure :: add_export_port => graph_add_export_port
      procedure :: add_child_port_binding => graph_add_child_port_binding
      procedure :: get_import_port => graph_get_import_port
      procedure :: get_export_port => graph_get_export_port
      procedure :: get_child_port_binding => graph_get_child_port_binding
      ! openspec/changes/graph-builder-stateitem-resolution, tasks.md
      ! 3.1: full-table enumeration counterparts of the single-key
      ! lookups above - for_each_matching_import's replacement body
      ! needs to walk every bound import port of a destination
      ! ComponentGraph, not just look up one by a caller-supplied PortId
      ! (which it never has in hand before the match). Same "return a
      ! copy, empty once finalized" contract as get_node_ids() below.
      procedure :: get_import_ports => graph_get_import_ports
      procedure :: get_export_ports => graph_get_export_ports
      ! -- transform port bindings (REQ-XFORM-005) --------------------------
      procedure :: bind_port => graph_bind_port
      procedure :: get_port_binding => graph_get_port_binding
      procedure :: get_port_bindings => graph_get_port_bindings
      procedure :: add_resource_index => graph_add_resource_index
      procedure :: get_resource_index => graph_get_resource_index
      ! -- full-graph enumeration (for exporters / full-graph walks) --------
      procedure :: get_node_ids => graph_get_node_ids
      procedure :: get_network_ids => graph_get_network_ids
      ! -- demand-driven update (spec/11-revision-and-update.md REQ-REV-005..007) --
      procedure :: update => graph_update
      ! -- validation / lifecycle -------------------------------------------
      procedure :: validate => graph_validate
      procedure :: freeze => graph_freeze
      procedure :: is_frozen => graph_is_frozen
      procedure :: finalize => graph_finalize
      procedure :: is_finalized => graph_is_finalized
      ! -- private helpers ---------------------------------------------------
      procedure, private :: classify_owned_nodes => graph_classify_owned_nodes
   end type ComponentGraph

   interface ComponentGraph
      module procedure new_ComponentGraph
   end interface ComponentGraph

   ! One explicit-stack frame for graph_update's iterative traversal
   ! (mapl_ComponentGraph_DemandDrivenUpdate_smod, below) - not itself
   ! part of ComponentGraph's own public API. Deliberately declared here
   ! in the ancestor module rather than inside that submodule: this is
   ! the only submodule anywhere in this codebase that would otherwise
   ! declare its own module-level derived type, and doing so triggered a
   ! shared-library symbol-versioning gap under ifx (the auto-generated
   ! .so version script had no entry for the type's compiler-generated
   ! descriptor symbol, whose name encodes both the ancestor module and
   ! submodule - `ld: version node not found for symbol ...@..._UPDATEFRAME`).
   ! Every other submodule in the codebase only implements interface
   ! bodies, never declares its own types - matching that established,
   ! universally-working pattern here avoids relying on an apparently
   ! untested corner of the toolchain, rather than chasing the exact
   ! root cause in ifx/CMake's version-script generation.
   type :: UpdateFrame
      type(NodeId) :: node_id
      class(GraphNode), pointer :: node => null()
      type(NodeId), allocatable :: predecessor_ids(:)
      integer :: next_index = 1
   end type UpdateFrame

   ! update()'s implementation (post-order dependency-graph traversal,
   ! REQ-REV-005..007 - explicit-stack, not literal Fortran recursion;
   ! see that submodule's own header comment) lives in a separate
   ! file/submodule
   ! (mapl_ComponentGraph_DemandDrivenUpdate_smod,
   ! ComponentGraph_DemandDrivenUpdate.F90) rather than inline below,
   ! purely to keep this already-large file from growing further - a
   ! submodule is still "the same module" for every visibility/API
   ! purpose (callers see an ordinary `graph%update(...)` type-bound
   ! call; there is no separate public interface to maintain).
   interface
      module subroutine graph_update(this, network_id, node_id, rc)
         class(ComponentGraph), target, intent(in) :: this
         type(DependencyNetworkId), intent(in) :: network_id
         type(NodeId), intent(in) :: node_id
         integer, optional, intent(out) :: rc
      end subroutine graph_update
   end interface

contains

   ! REQ-CG-003a/004: structure constructor performs the unconditional
   ! bootstrap (the default network's id is the first DependencyNetworkId
   ! issued by this hierarchy's own shared generator and cannot hit
   ! exhaustion this early).
   !
   ! openspec/changes/hierarchy-wide-component-graph, design.md D1: when
   ! hierarchy_id is omitted (today's existing ~40 standalone-synthetic-
   ! graph test call shape, `graph = ComponentGraph()`), this constructor
   ! requests a fresh hierarchy of its own via request_new_hierarchy() -
   ! every existing no-argument test call site keeps working unchanged,
   ! each one implicitly becoming the root of its own one-component
   ! "hierarchy." A real OuterMetaComponent always supplies its own
   ! hierarchy_id explicitly (threaded down from the hierarchy's root at
   ! construction time - see OuterMetaComponent/init_meta.F90), so every
   ! ComponentGraph in a real hierarchy shares the same HierarchyGraph.
   function new_ComponentGraph(hierarchy_id) result(graph)
      type(HierarchyId), optional, intent(in) :: hierarchy_id
      type(ComponentGraph) :: graph

      type(HierarchyResources), pointer :: resources
      type(DependencyNetwork) :: empty_network

      if (present(hierarchy_id)) then
         graph%hierarchy_id = hierarchy_id
      else
         graph%hierarchy_id = request_new_hierarchy()
      end if

      resources => get_hierarchy_resources(graph%hierarchy_id)
      graph%default_network_id = resources%graph%next_network_id()
      call resources%graph%register_network(graph%default_network_id, empty_network)
      call graph%own_network_ids%insert(graph%default_network_id)
   end function new_ComponentGraph

   ! -- node registry -------------------------------------------------------

   function graph_next_node_id(this, rc) result(id)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(NodeId) :: id

      type(HierarchyResources), pointer :: resources
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: next_node_id called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: next_node_id called on a frozen graph')

      resources => get_hierarchy_resources(this%hierarchy_id)
      id = resources%graph%next_node_id(status)
      _ASSERT(status == 0, 'ComponentGraph: NodeId generator exhausted')

      _RETURN(_SUCCESS)
   end function graph_next_node_id

   ! REQ-CG-001/REQ-ID-005: the shared HierarchyGraph is authoritative; a
   ! node cannot make itself a member by claiming an id, and no second
   ! node silently replaces an already-registered one, anywhere in the
   ! hierarchy. own_node_ids additionally records that THIS instance is
   ! the one that registered id (design.md D2) - the explicit ownership
   ! fact that used to be implicit from private-map membership.
   subroutine graph_register_node(this, node, rc)
      class(ComponentGraph), intent(inout) :: this
      class(GraphNode), target, intent(in) :: node
      integer, optional, intent(out) :: rc

      type(HierarchyResources), pointer :: resources
      type(NodeId) :: id
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: register_node called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: register_node called on a frozen graph')

      id = node%get_node_id()
      _ASSERT(id%is_valid(), 'ComponentGraph: register_node requires a NodeId obtained via next_node_id()')

      resources => get_hierarchy_resources(this%hierarchy_id)
      call resources%graph%register_node(node, status)
      _ASSERT(status == 0, 'ComponentGraph: register_node - NodeId is already registered')

      call this%own_node_ids%insert(id)

      _RETURN(_SUCCESS)
   end subroutine graph_register_node

   ! Any node anywhere in the hierarchy is retrievable by its NodeId
   ! (graph/component-hierarchy's "Two components' nodes live in one
   ! shared store" scenario) - no ownership filtering here; owns_node()
   ! below is the ownership-scoped query.
   function graph_get_node(this, id) result(node)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeId), intent(in) :: id
      class(GraphNode), pointer :: node

      type(HierarchyResources), pointer :: resources

      node => null()
      if (this%is_finalized()) return
      resources => get_hierarchy_resources(this%hierarchy_id)
      node => resources%graph%get_node(id)
   end function graph_get_node

   ! design.md D2: ownership is explicit (this instance's own
   ! own_node_ids record), not inferred from presence in the shared
   ! HierarchyGraph - every component's nodes are present there.
   logical function graph_owns_node(this, id) result(owns)
      class(ComponentGraph), intent(in) :: this
      type(NodeId), intent(in) :: id

      owns = (.not. this%is_finalized()) .and. (this%own_node_ids%count(id) > 0)
   end function graph_owns_node

   ! -- network registry ------------------------------------------------------

   function graph_get_default_network_id(this) result(id)
      class(ComponentGraph), intent(in) :: this
      type(DependencyNetworkId) :: id

      id = this%default_network_id
   end function graph_get_default_network_id

   ! REQ-CG-006: no new DependencyNetworks may be created after freeze.
   function graph_create_network(this, rc) result(id)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(DependencyNetworkId) :: id

      type(HierarchyResources), pointer :: resources
      type(DependencyNetwork) :: empty_network
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: create_network called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: create_network called on a frozen graph')

      resources => get_hierarchy_resources(this%hierarchy_id)
      id = resources%graph%next_network_id(status)
      _ASSERT(status == 0, 'ComponentGraph: DependencyNetworkId generator exhausted')

      call resources%graph%register_network(id, empty_network, status)
      _ASSERT(status == 0, 'ComponentGraph: create_network - DependencyNetworkId is already registered')

      call this%own_network_ids%insert(id)

      _RETURN(_SUCCESS)
   end function graph_create_network

   ! Any network anywhere in the hierarchy is retrievable by its
   ! DependencyNetworkId - no ownership filtering here; owns_network()
   ! below is the ownership-scoped query.
   function graph_get_network(this, id) result(network)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: id
      type(DependencyNetwork), pointer :: network

      type(HierarchyResources), pointer :: resources

      network => null()
      if (this%is_finalized()) return
      resources => get_hierarchy_resources(this%hierarchy_id)
      network => resources%graph%get_network(id)
   end function graph_get_network

   ! design.md D2: same explicit-ownership treatment as owns_node() above.
   logical function graph_owns_network(this, id) result(owns)
      class(ComponentGraph), intent(in) :: this
      type(DependencyNetworkId), intent(in) :: id

      owns = (.not. this%is_finalized()) .and. (this%own_network_ids%count(id) > 0)
   end function graph_owns_network

   ! Convenience wrapper: forwards to DependencyNetwork%add_dependency()
   ! with an allowed-id set built fresh for this one call (never cached),
   ! so callers get the REQ-CG-004 "Invalid foreign node is rejected"
   ! scenario for free. network_id must be owned by THIS instance
   ! (REQ-CG-001: which network a declared connection populates stays
   ! exactly as declaration-scoped as before shared storage - proposal.md
   ! Non-Goals) - an explicit check, since the shared HierarchyGraph
   ! would otherwise resolve ANY component's network_id, not only this
   ! instance's own.
   !
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 3.2: with
   ! proxy nodes retired, source/target MAY now be a different
   ! component's own real NodeId (resolved via GraphBuilder's
   ! resolve_qualified_component, whose one-directional tree walk is the
   ! actual enforcement of "never resolves upward past the declaring
   ! component" - graph/component-hierarchy's own requirement; see that
   ! requirement's own text for why ComponentGraph itself, which cannot
   ! depend on OuterComponent/the hierarchy tree per REQ-CG-002, is not
   ! the layer that re-derives that invariant). This instance's own
   ! own_node_ids remains the primary allowed set; a source/target this
   ! instance does not itself own is additionally permitted ONLY if it is
   ! genuinely registered somewhere in this exact hierarchy (allowed_foreign_node,
   ! below) - which only ever holds for an id some ComponentGraph in this
   ! hierarchy legitimately obtained via next_node_id()/register_node(),
   ! never an arbitrary or garbage value. This is deliberately NOT "any
   ! id in the shared store is unconditionally valid" - a truly foreign
   ! id (never registered anywhere in this hierarchy) is still rejected.
   subroutine graph_add_dependency(this, network_id, source, target, rc)
      class(ComponentGraph), target, intent(inout) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: source
      type(NodeId), intent(in) :: target
      integer, optional, intent(out) :: rc

      type(DependencyNetwork), pointer :: network
      type(NodeIdSet) :: allowed_ids

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_dependency called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_dependency called on a frozen graph')
      _ASSERT(this%owns_network(network_id), 'ComponentGraph: add_dependency - unknown DependencyNetworkId')

      network => this%get_network(network_id)
      _ASSERT(associated(network), 'ComponentGraph: add_dependency - unknown DependencyNetworkId')

      allowed_ids = this%own_node_ids
      if (.not. this%owns_node(source)) then
         _ASSERT(this%allowed_foreign_node(source), 'ComponentGraph: add_dependency - source NodeId is not registered anywhere in this hierarchy')
         call allowed_ids%insert(source)
      end if
      if (.not. this%owns_node(target)) then
         _ASSERT(this%allowed_foreign_node(target), 'ComponentGraph: add_dependency - target NodeId is not registered anywhere in this hierarchy')
         call allowed_ids%insert(target)
      end if

      call network%add_dependency(source, target, rc=rc, valid_ids=allowed_ids)
   end subroutine graph_add_dependency

   ! True iff id is registered in this hierarchy's shared HierarchyGraph
   ! (by ANY component), regardless of whether THIS instance owns it.
   ! Used only to admit an already-resolved cross-component edge endpoint
   ! (tasks.md 3.2) - never to answer "is this mine" (owns_node() already
   ! answers that) and never to re-derive the component-tree-shaped
   ! "descendant only" invariant, which resolve_qualified_component alone
   ! enforces (graph/component-hierarchy's "Connection endpoint
   ! resolution never resolves upward past its declaring component").
   logical function graph_allowed_foreign_node(this, id) result(allowed)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeId), intent(in) :: id

      class(GraphNode), pointer :: node

      node => this%get_node(id)
      allowed = associated(node)
   end function graph_allowed_foreign_node

   ! -- ports / bindings / resource index ---------------------------------

   function graph_next_port_id(this, rc) result(id)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(PortId) :: id

      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: next_port_id called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: next_port_id called on a frozen graph')

      id = this%port_generator%next(status)
      _ASSERT(status == 0, 'ComponentGraph: PortId generator exhausted')

      _RETURN(_SUCCESS)
   end function graph_next_port_id

   ! REQ-CG-001/005/006: not finalized, not frozen, port_id valid and not
   ! already bound (no silent replace), node_id owned by this instance
   ! (design.md D2 - owns_node(), not mere presence in the shared
   ! HierarchyGraph).
   subroutine graph_add_import_port(this, port_id, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_import_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_import_port called on a frozen graph')
      _ASSERT(port_id%is_valid(), 'ComponentGraph: add_import_port requires a PortId obtained via next_port_id()')
      _ASSERT(this%ports%import_ports%count(port_id) == 0, 'ComponentGraph: add_import_port - PortId is already bound')
      _ASSERT(this%owns_node(node_id), 'ComponentGraph: add_import_port - NodeId is not owned by this graph')

      call this%ports%import_ports%insert(port_id, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_import_port

   subroutine graph_add_export_port(this, port_id, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_export_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_export_port called on a frozen graph')
      _ASSERT(port_id%is_valid(), 'ComponentGraph: add_export_port requires a PortId obtained via next_port_id()')
      _ASSERT(this%ports%export_ports%count(port_id) == 0, 'ComponentGraph: add_export_port - PortId is already bound')
      _ASSERT(this%owns_node(node_id), 'ComponentGraph: add_export_port - NodeId is not owned by this graph')

      call this%ports%export_ports%insert(port_id, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_export_port

   subroutine graph_add_child_port_binding(this, port_id, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_child_port_binding called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_child_port_binding called on a frozen graph')
      _ASSERT(port_id%is_valid(), 'ComponentGraph: add_child_port_binding requires a PortId obtained via next_port_id()')
      _ASSERT(this%ports%child_bindings%count(port_id) == 0, 'ComponentGraph: add_child_port_binding - PortId is already bound')
      _ASSERT(this%owns_node(node_id), 'ComponentGraph: add_child_port_binding - NodeId is not owned by this graph')

      call this%ports%child_bindings%insert(port_id, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_child_port_binding

   function graph_get_import_port(this, port_id) result(node_id)
      class(ComponentGraph), target, intent(in) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), pointer :: node_id

      node_id => null()
      if (this%is_finalized()) return
      node_id => this%ports%import_ports%at(port_id)
   end function graph_get_import_port

   function graph_get_export_port(this, port_id) result(node_id)
      class(ComponentGraph), target, intent(in) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), pointer :: node_id

      node_id => null()
      if (this%is_finalized()) return
      node_id => this%ports%export_ports%at(port_id)
   end function graph_get_export_port

   function graph_get_child_port_binding(this, port_id) result(node_id)
      class(ComponentGraph), target, intent(in) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), pointer :: node_id

      node_id => null()
      if (this%is_finalized()) return
      node_id => this%ports%child_bindings%at(port_id)
   end function graph_get_child_port_binding

   ! openspec/changes/graph-builder-stateitem-resolution, tasks.md 3.1:
   ! full import-port table, by value (same "return a copy" convention
   ! as get_node_ids()/field_bundle_members()/state_members() elsewhere
   ! in this codebase) - empty once finalized, matching every other
   ! post-finalize accessor in this type.
   function graph_get_import_ports(this) result(ports)
      class(ComponentGraph), target, intent(in) :: this
      type(PortIdNodeIdMap) :: ports

      if (this%is_finalized()) return
      ports = this%ports%import_ports
   end function graph_get_import_ports

   function graph_get_export_ports(this) result(ports)
      class(ComponentGraph), target, intent(in) :: this
      type(PortIdNodeIdMap) :: ports

      if (this%is_finalized()) return
      ports = this%ports%export_ports
   end function graph_get_export_ports

   ! -- transform port bindings (spec/10-transforms-and-ports.md REQ-XFORM-005) --

   ! Validates: graph not finalized/frozen; network_id and both node ids
   ! owned by this instance (design.md D2); node_id refers to a
   ! TransformGraphNode that has actually declared port_name (as either
   ! an input or output - REQ-XFORM-004's "binding requires a matching
   ! declaration"); if that declaration is kind-constrained and
   ! target_id is a StateItemNode, its GraphStateItem%variant() matches
   ! the declared expectation. Delegates the actual no-silent-replace
   ! storage to PortBindingTable%bind().
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 3.2: a
   ! port's bound target_id MAY now be a different component's own
   ! real NodeId (e.g. find_or_build_extension_chain's own first
   ! INPUT_PORT_NAME binding, when export_node_id is a resolved
   ! descendant's own real export) - same "registered somewhere in
   ! this hierarchy, not necessarily owned by this instance" admission
   ! graph_add_dependency already applies (see that procedure's own
   ! header comment); node_id (the TransformGraphNode itself) is always
   ! this instance's own, unaffected.
   subroutine graph_bind_port(this, network_id, node_id, port_name, target_id, rc)
      class(ComponentGraph), target, intent(inout) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name
      type(NodeId), intent(in) :: target_id
      integer, optional, intent(out) :: rc

      class(GraphNode), pointer :: node, target_node
      logical :: is_input, is_output
      type(PortSpec) :: spec
      type(MAPL_StateItem_Flag) :: expected_kind, actual_kind
      type(GraphStateItem) :: target_payload
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: bind_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: bind_port called on a frozen graph')
      _ASSERT(this%owns_network(network_id), 'ComponentGraph: bind_port - unknown DependencyNetworkId')
      _ASSERT(this%owns_node(node_id), 'ComponentGraph: bind_port - transform NodeId is not owned by this graph')
      if (.not. this%owns_node(target_id)) then
         _ASSERT(this%allowed_foreign_node(target_id), 'ComponentGraph: bind_port - target NodeId is not registered anywhere in this hierarchy')
      end if

      node => this%get_node(node_id)
      select type (node)
      class is (TransformGraphNode)
         is_input = node%is_input_port(port_name)
         is_output = node%is_output_port(port_name)
         _ASSERT(is_input .or. is_output, 'ComponentGraph: bind_port - port name not declared by this TransformGraphNode')

         if (is_input) then
            spec = node%get_input_port(port_name, _RC)
         else
            spec = node%get_output_port(port_name, _RC)
         end if

         if (spec%is_kind_constrained()) then
            expected_kind = spec%get_expected_kind(_RC)
            target_node => this%get_node(target_id)
            select type (target_node)
            class is (StateItemNode)
               target_payload = target_node%get_payload()
               actual_kind = target_payload%variant(_RC)
               _ASSERT(actual_kind == expected_kind, 'ComponentGraph: bind_port - bound NodeId kind does not match declared port kind constraint')
            class default
               ! Binding target is not a StateItemNode: the kind
               ! constraint is not checkable at this layer - documented
               ! intentional no-op, not an oversight. In practice every
               ! port binding target is expected to be a StateItemNode.
            end select
         end if
      class default
         _ASSERT(.false., 'ComponentGraph: bind_port - node_id does not refer to a TransformGraphNode')
      end select

      call this%transform_port_bindings%bind(network_id, node_id, port_name, target_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine graph_bind_port

   function graph_get_port_binding(this, network_id, node_id, port_name) result(target_id)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name
      type(NodeId), pointer :: target_id

      target_id => null()
      if (this%is_finalized()) return
      target_id => this%transform_port_bindings%get_binding(network_id, node_id, port_name)
   end function graph_get_port_binding

   function graph_get_port_bindings(this, network_id, node_id) result(port_map)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      type(StateItemMemberMap) :: port_map

      if (this%is_finalized()) return
      port_map = this%transform_port_bindings%get_bindings(network_id, node_id)
   end function graph_get_port_bindings

   subroutine graph_add_resource_index(this, key, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      character(*), intent(in) :: key
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_resource_index called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_resource_index called on a frozen graph')
      _ASSERT(this%owns_node(node_id), 'ComponentGraph: add_resource_index - NodeId is not owned by this graph')
      _ASSERT(this%resource_index%count(key) == 0, 'ComponentGraph: add_resource_index - key is already bound')

      call this%resource_index%insert(key, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_resource_index

   function graph_get_resource_index(this, key) result(node_id)
      class(ComponentGraph), target, intent(in) :: this
      character(*), intent(in) :: key
      type(NodeId), pointer :: node_id

      node_id => null()
      if (this%is_finalized()) return
      node_id => this%resource_index%at(key)
   end function graph_get_resource_index

   ! -- validation / lifecycle -----------------------------------------------

   ! REQ-CG-004 scenario "Invalid foreign node is rejected" and "Same-pass
   ! cross-network write is rejected": validates every network this
   ! instance owns (passing this instance's own ownership/producer/
   ! state-item classification sets down to DependencyNetwork%validate()),
   ! then checks REQ-DEP-008a across all of this instance's own networks.
   !
   ! openspec/changes/hierarchy-wide-component-graph, tasks.md 3.2: each
   ! owned network's own allowed-id set is own_node_ids plus whichever of
   ! that one network's own referenced ids are genuinely registered
   ! elsewhere in this hierarchy (graph_allowed_foreign_node) - same
   ! "registered somewhere in this hierarchy, not merely owned by this
   ! instance" admission graph_add_dependency already applies at wiring
   ! time, re-derived here (not cached) since validate() may run standalone
   ! (e.g. a synthetic test with no GraphBuilder/add_dependency call
   ! sequence at all).
   subroutine graph_validate(this, rc)
      class(ComponentGraph), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      type(NodeIdSet) :: producer_ids, state_item_ids
      type(NodeIdSet) :: allowed_ids
      type(NodeIdSet), target :: referenced_ids
      type(NodeIdSetIterator) :: ref_iter
      type(NodeId) :: ref_id
      type(DependencyNetworkIdSetIterator) :: net_iter
      type(DependencyNetworkId) :: net_id
      type(DependencyNetwork), pointer :: network
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: validate called after finalize')

      call this%classify_owned_nodes(producer_ids, state_item_ids)

      net_iter = this%own_network_ids%ftn_begin()
      do while (net_iter /= this%own_network_ids%ftn_end())
         call net_iter%next()
         net_id = net_iter%of()
         network => this%get_network(net_id)
         _ASSERT(associated(network), 'ComponentGraph: validate - owned DependencyNetworkId not found')

         allowed_ids = this%own_node_ids
         referenced_ids = network%get_referenced_ids()
         ref_iter = referenced_ids%ftn_begin()
         do while (ref_iter /= referenced_ids%ftn_end())
            call ref_iter%next()
            ref_id = ref_iter%of()
            if (this%owns_node(ref_id)) cycle
            _ASSERT(this%allowed_foreign_node(ref_id), 'ComponentGraph: validate - referenced NodeId is not registered anywhere in this hierarchy')
            call allowed_ids%insert(ref_id)
         end do

         call network%validate(valid_ids=allowed_ids, producer_ids=producer_ids, &
                                 state_item_ids=state_item_ids, _RC)
      end do

      call validate_cross_network_writes(this, state_item_ids, _RC)

      _RETURN(_SUCCESS)
   end subroutine graph_validate

   ! REQ-DEP-008a: across all networks this instance owns, no
   ! StateItemNode may be written (have a non-empty predecessor set) in
   ! more than one network.
   subroutine validate_cross_network_writes(this, state_item_ids, rc)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet), target, intent(in) :: state_item_ids
      integer, optional, intent(out) :: rc

      type(NodeIdSetIterator) :: item_iter
      type(DependencyNetworkIdSetIterator) :: net_iter
      type(NodeId) :: item
      type(DependencyNetworkId) :: net_id
      type(DependencyNetwork), pointer :: network
      integer :: write_count

      item_iter = state_item_ids%ftn_begin()
      do while (item_iter /= state_item_ids%ftn_end())
         call item_iter%next()
         item = item_iter%of()

         write_count = 0
         net_iter = this%own_network_ids%ftn_begin()
         do while (net_iter /= this%own_network_ids%ftn_end())
            call net_iter%next()
            net_id = net_iter%of()
            network => this%get_network(net_id)
            if (associated(network)) then
               if (network%has_predecessors(item)) write_count = write_count + 1
            end if
         end do

         _ASSERT(write_count <= 1, 'ComponentGraph: validate found a state item written in more than one network within the same update pass')
      end do

      _RETURN(_SUCCESS)
   end subroutine validate_cross_network_writes

   ! REQ-CG-005..007: validates first (no freeze state changes on
   ! failure), then freezes every network this instance owns, then the
   ! instance itself. Idempotent if already frozen.
   subroutine graph_freeze(this, rc)
      class(ComponentGraph), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(DependencyNetworkIdSetIterator) :: net_iter
      type(DependencyNetworkId) :: net_id
      type(DependencyNetwork), pointer :: network
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: freeze called after finalize')

      if (this%is_frozen()) then
         _RETURN(_SUCCESS)
      end if

      call this%validate(_RC)

      net_iter = this%own_network_ids%ftn_begin()
      do while (net_iter /= this%own_network_ids%ftn_end())
         call net_iter%next()
         net_id = net_iter%of()
         network => this%get_network(net_id)
         if (associated(network)) call network%freeze()
      end do

      this%lifecycle_status = GRAPH_LIFECYCLE_FROZEN

      _RETURN(_SUCCESS)
   end subroutine graph_freeze

   ! Frozen and finalized are both "structure is immutable" states -
   ! finalize() may be called directly from mutable (REQ-CG-003b does not
   ! require a prior freeze), so this reports true for either.
   logical function graph_is_frozen(this) result(frozen)
      class(ComponentGraph), intent(in) :: this

      frozen = this%lifecycle_status /= GRAPH_LIFECYCLE_MUTABLE
   end function graph_is_frozen

   ! REQ-CG-003b/003c: explicit, fallible, never folded into a destructor.
   subroutine graph_finalize(this, rc)
      class(ComponentGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: finalize called on an already-finalized graph')

      this%lifecycle_status = GRAPH_LIFECYCLE_FINALIZED

      _RETURN(_SUCCESS)
   end subroutine graph_finalize

   logical function graph_is_finalized(this) result(finalized)
      class(ComponentGraph), intent(in) :: this

      finalized = this%lifecycle_status == GRAPH_LIFECYCLE_FINALIZED
   end function graph_is_finalized

   ! -- full-graph enumeration (public; needed by the graph-neutral --------
   ! -- exporter, spec/19-visualization-export.md REQ-VIZ-003, and any -----
   ! -- other caller that must walk the whole graph via public API only) ---

   ! Returns a copy of this instance's own explicit ownership record
   ! (design.md D2), empty (rather than erroring) once finalized, matching
   ! every other post-finalize accessor in this type.
   function graph_get_node_ids(this) result(ids)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet) :: ids

      if (this%is_finalized()) return
      ids = this%own_node_ids
   end function graph_get_node_ids

   function graph_get_network_ids(this) result(ids)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkIdSet) :: ids

      if (this%is_finalized()) return
      ids = this%own_network_ids
   end function graph_get_network_ids

   ! -- private helpers --------------------------------------------------------

   ! Classifies owned nodes for producer/state-item purposes (REQ-DEP-008):
   ! an OperationGraphNode descendant is a producer-capable node; a
   ! StateItemNode is subject to the one-producer-per-state-item check.
   ! Walks this instance's own explicit own_node_ids record (design.md
   ! D2), fetching each node from the shared HierarchyGraph via the
   ! public get_node() accessor.
   subroutine graph_classify_owned_nodes(this, producer_ids, state_item_ids)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet), intent(out) :: producer_ids
      type(NodeIdSet), intent(out) :: state_item_ids

      type(NodeIdSetIterator) :: iter
      type(NodeId) :: id
      class(GraphNode), pointer :: node

      iter = this%own_node_ids%ftn_begin()
      do while (iter /= this%own_node_ids%ftn_end())
         call iter%next()
         id = iter%of()
         node => this%get_node(id)
         if (.not. associated(node)) cycle
         select type (node)
         class is (OperationGraphNode)
            call producer_ids%insert(id)
         class is (StateItemNode)
            call state_item_ids%insert(id)
         class default
            ! Any other GraphNode kind (e.g. a bare BaseGraphNode with no
            ! more specific role) is neither producer-capable nor subject
            ! to the one-producer-per-state-item constraint - deliberately
            ! left unclassified, not an oversight.
         end select
      end do
   end subroutine graph_classify_owned_nodes

end module mapl_ComponentGraph_mod
