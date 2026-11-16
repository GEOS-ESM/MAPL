#include "MAPL.h"

!------------------------------------------------------------------------------
! ComponentGraph: graph-neutral owner of a component's own public import/
! export ports, advertised-item resource index,
! write-back networks, port-identity generation, and mutable/frozen/finalized
! lifecycle state.
!
! The polymorphic node map (NodeId -> GraphNode), the dependency-network
! map (DependencyNetworkId -> DependencyNetwork), the node/network id
! generators, the one forward network, the transform port-binding table and
! the reuse index live in one HierarchyGraph per hierarchy, held in the
! HierarchyId-keyed registry (mapl_HierarchyRegistry_mod). This type holds
! only a HierarchyId and resolves the shared storage through
! get_hierarchy_resources() wherever needed, never caching a reference.
! Each instance also keeps explicit NodeIdSet/DependencyNetworkIdSet
! records of the ids it registered itself: "owned" means "recorded in this
! instance's set", since every component's ids share the one HierarchyGraph.
!
! MUST NOT depend on OuterComponent, StateRegistry, GriddedComponentDriver,
! GraphBuilder, or any component-hierarchy type.
!
! Transform-port declarations live on TransformGraphNode; the external
! port binding table (bind_port()/get_port_binding()/get_port_bindings(),
! keyed (DependencyNetworkId, NodeId) -> port name -> NodeId) is shared by
! the hierarchy (external storage, never on-node).
!
! update() is type-bound here but implemented in the
! submodule ComponentGraph_DemandDrivenUpdate.F90 purely for file size.
! mapl_GraphExport_mod, by contrast, is deliberately a separate module: a
! submodule can reach private components, which would void the "depends only
! on the public query API" guarantee of REQ-VIZ-003.
!
! PortId is the only per-instance generator. The two PortId -> NodeId
! tables (import ports, export ports) and the export-name side table are
! bundled in the private GraphPortRegistry. lifecycle_status is a single
! mutable/frozen/finalized state; see graph_freeze() and graph_finalize()
! for the only two transitions.
!
! Iteration convention: gFTL walks use ftn_begin()/ftn_end() with the
! %next() call at the top of the loop body (iter starts one position before
! the first element).
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
   use mapl_PortIdStringMap_mod, only: PortIdStringMap
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

   ! Private aggregation of the PortId -> NodeId lookup tables (import
   ! ports, export ports) and the export-name side table.
   !
   ! export_port_names is a sparse side-table, PortId -> overriding qualified
   ! name, populated only for export ports created by export propagation, never
   ! for directly advertised ones (those match on the node's own unqualified
   ! short_name). find_export_node (GraphBuilder_ConnectionResolution_smod.F90)
   ! is its sole reader, via graph_get_export_port_name.
   type :: GraphPortRegistry
      private
      type(PortIdNodeIdMap) :: import_ports
      type(PortIdNodeIdMap) :: export_ports
      type(PortIdStringMap) :: export_port_names
   end type GraphPortRegistry

   type :: ComponentGraph
      private
      type(HierarchyId) :: hierarchy_id
      ! Explicit record of which ids this instance registered itself (not a
      ! reverse hierarchy-wide lookup); filled by graph_register_node/
      ! graph_create_network, read by owns_node()/owns_network().
      ! own_network_ids holds only write-back networks; the forward network
      ! belongs to the hierarchy.
      type(NodeIdSet) :: own_node_ids
      type(DependencyNetworkIdSet) :: own_network_ids
      type(PortIdGenerator) :: port_generator
      type(GraphPortRegistry) :: ports
      type(StateItemMemberMap) :: resource_index
      integer :: lifecycle_status = GRAPH_LIFECYCLE_MUTABLE
   contains
      ! -- node registry --------------------------------------------------
      procedure :: next_node_id => graph_next_node_id
      procedure :: register_node => graph_register_node
      procedure :: get_node => graph_get_node
      procedure :: owns_node => graph_owns_node
      ! registered somewhere in this hierarchy, not necessarily by this
      ! instance (see graph_add_dependency)
      procedure :: allowed_foreign_node => graph_allowed_foreign_node
      ! -- network registry ------------------------------------------------
      procedure :: get_forward_network_id => graph_get_forward_network_id
      procedure :: is_forward_network => graph_is_forward_network
      procedure :: create_network => graph_create_network
      procedure :: get_network => graph_get_network
      procedure :: owns_network => graph_owns_network
      procedure :: add_dependency => graph_add_dependency
      ! -- ports / bindings / resource index --------------------------------
      procedure :: next_port_id => graph_next_port_id
      procedure :: add_import_port => graph_add_import_port
      procedure :: add_export_port => graph_add_export_port
      ! the name an export port matches connections by: the export-propagation
      ! override if one exists, else the node's own unqualified short_name
      procedure :: get_export_port_name => graph_get_export_port_name
      ! full-table enumeration: same "return a copy, empty once finalized"
      ! contract as get_node_ids()
      procedure :: get_import_ports => graph_get_import_ports
      procedure :: get_export_ports => graph_get_export_ports
      ! -- transform port bindings --------------------------
      procedure :: bind_port => graph_bind_port
      procedure :: get_port_binding => graph_get_port_binding
      procedure :: get_port_bindings => graph_get_port_bindings
      procedure :: add_resource_index => graph_add_resource_index
      procedure :: get_resource_index => graph_get_resource_index
      procedure :: add_reuse_entry => graph_add_reuse_entry
      procedure :: forward_producer_conflict => graph_forward_producer_conflict
      procedure :: get_reuse_entry => graph_get_reuse_entry
      ! -- full-graph enumeration (for exporters / full-graph walks) --------
      procedure :: get_node_ids => graph_get_node_ids
      procedure :: get_network_ids => graph_get_network_ids
      ! -- demand-driven update --
      procedure :: update => graph_update_item
      ! -- validation / lifecycle -------------------------------------------
      procedure :: validate => graph_validate
      procedure :: freeze => graph_freeze
      ! hierarchy-wide: called once, after every component graph froze
      procedure :: freeze_forward_network => graph_freeze_forward_network
      procedure :: is_frozen => graph_is_frozen
      procedure :: finalize => graph_finalize
      procedure :: is_finalized => graph_is_finalized
      ! -- private helpers ---------------------------------------------------
      procedure, private :: classify_nodes => graph_classify_nodes
   end type ComponentGraph

   interface ComponentGraph
      module procedure new_ComponentGraph
   end interface ComponentGraph

   ! One explicit-stack frame for graph_update_item's iterative traversal; not
   ! part of the public API. Declared here rather than in the submodule because
   ! a submodule-local derived type broke the ifx shared-library version script
   ! (`version node not found for symbol ...@..._UPDATEFRAME`).
   type :: UpdateFrame
      type(NodeId) :: node_id
      class(GraphNode), pointer :: node => null()
      type(NodeId), allocatable :: predecessor_ids(:)
      integer :: next_index = 1
   end type UpdateFrame

   ! The walk behind update() is implemented in
   ! mapl_ComponentGraph_DemandDrivenUpdate_smod
   ! (ComponentGraph_DemandDrivenUpdate.F90) to keep this file from growing;
   ! callers see an ordinary graph%update(node_id) type-bound call.
   interface
      module subroutine graph_update_item(this, node_id, rc)
         class(ComponentGraph), target, intent(in) :: this
         type(NodeId), intent(in) :: node_id
         integer, optional, intent(out) :: rc
      end subroutine graph_update_item
   end interface

contains

   ! When hierarchy_id is omitted (standalone synthetic graphs in tests), a
   ! fresh hierarchy is requested and this graph becomes its root. A real
   ! OuterMetaComponent always passes the hierarchy's hierarchy_id (see
   ! OuterMetaComponent/init_meta.F90), so all its ComponentGraphs share one
   ! HierarchyGraph.
   function new_ComponentGraph(hierarchy_id) result(graph)
      type(HierarchyId), optional, intent(in) :: hierarchy_id
      type(ComponentGraph) :: graph

      if (present(hierarchy_id)) then
         graph%hierarchy_id = hierarchy_id
      else
         graph%hierarchy_id = request_new_hierarchy()
      end if
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

   ! The shared HierarchyGraph is authoritative; a
   ! node cannot make itself a member by claiming an id, and no second node
   ! silently replaces a registered one anywhere in the hierarchy.
   ! own_node_ids records that THIS instance registered id.
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

   ! Any node in the hierarchy is retrievable by NodeId, with no ownership
   ! filtering; owns_node() is the ownership-scoped query.
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

   ! Ownership is the explicit own_node_ids record, not presence in the
   ! shared HierarchyGraph (every component's nodes are present there).
   logical function graph_owns_node(this, id) result(owns)
      class(ComponentGraph), intent(in) :: this
      type(NodeId), intent(in) :: id

      owns = (.not. this%is_finalized()) .and. (this%own_node_ids%count(id) > 0)
   end function graph_owns_node

   ! -- network registry ------------------------------------------------------

   ! The hierarchy's one forward network: the same id from every
   ! ComponentGraph in the hierarchy.
   function graph_get_forward_network_id(this, rc) result(id)
      class(ComponentGraph), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(DependencyNetworkId) :: id

      type(HierarchyResources), pointer :: resources

      resources => get_hierarchy_resources(this%hierarchy_id)
      _ASSERT(associated(resources), 'ComponentGraph: get_forward_network_id - hierarchy not found')
      id = resources%graph%get_forward_network_id()

      _RETURN(_SUCCESS)
   end function graph_get_forward_network_id

   logical function graph_is_forward_network(this, id) result(is_forward)
      class(ComponentGraph), intent(in) :: this
      type(DependencyNetworkId), intent(in) :: id

      is_forward = (id == this%get_forward_network_id())
   end function graph_is_forward_network

   ! No new DependencyNetworks may be created after freeze.
   ! A created network is a write-back network owned by this instance.
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

   ! Any network in the hierarchy is retrievable by id, with no ownership
   ! filtering; owns_network() is the ownership-scoped query.
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

   ! Same explicit-ownership rule as owns_node().
   logical function graph_owns_network(this, id) result(owns)
      class(ComponentGraph), intent(in) :: this
      type(DependencyNetworkId), intent(in) :: id

      owns = (.not. this%is_finalized()) .and. (this%own_network_ids%count(id) > 0)
   end function graph_owns_network

   ! network_id is the hierarchy's forward network (accepted from any graph in
   ! the hierarchy) or a write-back network this instance created.
   !
   ! source/target may be another component's real NodeId (resolved by
   ! GraphBuilder's resolve_qualified_component, whose one-directional tree walk
   ! is what prevents resolving upward past the declaring component; this layer
   ! cannot know the component tree). Either endpoint must be
   ! registered somewhere in this hierarchy; an id never registered is rejected.
   !
   ! In the forward network a state item has at most one producer, whichever
   ! component declares the edge; re-adding the identical edge is a no-op,
   ! since two-pass initialization may resolve one connection twice.
   subroutine graph_add_dependency(this, network_id, source, target, rc)
      class(ComponentGraph), target, intent(inout) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: source
      type(NodeId), intent(in) :: target
      integer, optional, intent(out) :: rc

      type(DependencyNetwork), pointer :: network
      logical :: is_forward
      character(:), allocatable :: item_name
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_dependency called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_dependency called on a frozen graph')
      is_forward = this%is_forward_network(network_id)
      _ASSERT(is_forward .or. this%owns_network(network_id), 'ComponentGraph: add_dependency - unknown DependencyNetworkId')

      network => this%get_network(network_id)
      _ASSERT(associated(network), 'ComponentGraph: add_dependency - unknown DependencyNetworkId')
      _ASSERT(.not. network%is_frozen(), 'ComponentGraph: add_dependency called on a frozen network')

      _ASSERT(this%allowed_foreign_node(source), 'ComponentGraph: add_dependency - source NodeId is not registered anywhere in this hierarchy')
      _ASSERT(this%allowed_foreign_node(target), 'ComponentGraph: add_dependency - target NodeId is not registered anywhere in this hierarchy')

      if (network%contains_dependency(source, target)) then
         _RETURN(_SUCCESS)
      end if

      if (is_forward) then
         item_name = this%forward_producer_conflict(source, target)
         _ASSERT(item_name == '', 'ComponentGraph: forward network item "' // item_name // '" already has a different producer')
      end if

      call network%add_dependency(source, target, _RC)

      _RETURN(_SUCCESS)
   end subroutine graph_add_dependency

   ! Returns the item's name when adding source -> target to the forward
   ! network would give StateItemNode target a second, different producer;
   ! '' otherwise (no conflict, identical edge, or target not a state item).
   function graph_forward_producer_conflict(this, source, target) result(item_name)
      character(:), allocatable :: item_name
      class(ComponentGraph), target, intent(in) :: this
      type(NodeId), intent(in) :: source
      type(NodeId), intent(in) :: target

      class(GraphNode), pointer :: node
      type(DependencyNetwork), pointer :: network
      integer :: status

      item_name = ''
      network => this%get_network(this%get_forward_network_id(rc=status))
      if (status /= 0 .or. .not. associated(network)) return
      if (network%contains_dependency(source, target)) return
      if (.not. network%has_predecessors(target)) return
      node => this%get_node(target)
      if (.not. associated(node)) return
      select type (node)
      class is (StateItemNode)
         if (node%has_short_name()) then
            item_name = node%get_short_name()
         else
            item_name = target%to_string()
         end if
      class default
         ! Transforms and other operations may have many inputs.
      end select
   end function graph_forward_producer_conflict

   ! True iff id is registered in this hierarchy's shared HierarchyGraph by
   ! ANY component. Admits already-resolved cross-component edge endpoints;
   ! it does not answer "is this mine" (owns_node()) and does not enforce the
   ! "descendant only" rule (resolve_qualified_component does).
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

   ! Not finalized, not frozen, port_id valid and not
   ! already bound (no silent replace). node_id must be owned by this
   ! instance (owns_node(), not mere presence in the shared HierarchyGraph),
   ! or be a node registered elsewhere in this hierarchy: a mirrored import
   ! port's node is a child component's real NodeId.
   subroutine graph_add_import_port(this, port_id, node_id, rc)
      class(ComponentGraph), target, intent(inout) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_import_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_import_port called on a frozen graph')
      _ASSERT(port_id%is_valid(), 'ComponentGraph: add_import_port requires a PortId obtained via next_port_id()')
      _ASSERT(this%ports%import_ports%count(port_id) == 0, 'ComponentGraph: add_import_port - PortId is already bound')
      if (.not. this%owns_node(node_id)) then
         _ASSERT(this%allowed_foreign_node(node_id), 'ComponentGraph: add_import_port - NodeId is not registered anywhere in this hierarchy')
      end if

      call this%ports%import_ports%insert(port_id, node_id)

      _RETURN(_SUCCESS)
   end subroutine graph_add_import_port

   ! Optional qualified_name records an export-propagation override (see
   ! GraphPortRegistry); absent for a directly advertised export, present for
   ! a propagated one. As for import ports, a foreign node registered in this
   ! hierarchy is admitted: a propagated export's node is a descendant's.
   subroutine graph_add_export_port(this, port_id, node_id, rc, qualified_name)
      class(ComponentGraph), target, intent(inout) :: this
      type(PortId), intent(in) :: port_id
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc
      character(*), optional, intent(in) :: qualified_name

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_export_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_export_port called on a frozen graph')
      _ASSERT(port_id%is_valid(), 'ComponentGraph: add_export_port requires a PortId obtained via next_port_id()')
      _ASSERT(this%ports%export_ports%count(port_id) == 0, 'ComponentGraph: add_export_port - PortId is already bound')
      if (.not. this%owns_node(node_id)) then
         _ASSERT(this%allowed_foreign_node(node_id), 'ComponentGraph: add_export_port - NodeId is not registered anywhere in this hierarchy')
      end if

      call this%ports%export_ports%insert(port_id, node_id)
      if (present(qualified_name)) then
         call this%ports%export_port_names%insert(port_id, qualified_name)
      end if

      _RETURN(_SUCCESS)
   end subroutine graph_add_export_port

   ! The name an exact-match connection matches this export port against:
   ! the export-propagation override if one was recorded for port_id, else
   ! the node's own unqualified short_name. Returns '' if port_id is unbound
   ! or not a StateItemNode (not an export candidate).
   ! '' is never a valid export name (names come from a VariableSpec short
   ! name or child/name).
   function graph_get_export_port_name(this, port_id) result(name)
      class(ComponentGraph), target, intent(in) :: this
      type(PortId), intent(in) :: port_id
      character(:), allocatable :: name

      character(:), pointer :: override
      type(NodeId), pointer :: node_id
      class(GraphNode), pointer :: generic_node

      allocate(name, source='')
      if (this%is_finalized()) return

      override => this%ports%export_port_names%at(port_id)
      if (associated(override)) then
         name = override
         return
      end if

      node_id => this%ports%export_ports%at(port_id)
      if (.not. associated(node_id)) return
      generic_node => this%get_node(node_id)
      if (.not. associated(generic_node)) return
      select type (generic_node)
      class is (StateItemNode)
         name = generic_node%get_short_name()
      end select
   end function graph_get_export_port_name

   ! Full import-port table, returned by value; empty once finalized like
   ! every other post-finalize accessor.
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

   ! -- transform port bindings --

   ! Validates: graph not finalized/frozen; network_id is the forward network
   ! or owned by this instance, and not frozen; node_id is a TransformGraphNode
   ! registered in this hierarchy that declared port_name; if
   ! that declaration is kind-constrained and target_id is a StateItemNode, its
   ! GraphStateItem%variant() matches. Storage (no silent replace) is the
   ! hierarchy's shared PortBindingTable.
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
      type(DependencyNetwork), pointer :: network
      type(PortBindingTable), pointer :: table
      type(HierarchyResources), pointer :: resources
      logical :: is_forward
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: bind_port called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: bind_port called on a frozen graph')
      is_forward = this%is_forward_network(network_id)
      _ASSERT(is_forward .or. this%owns_network(network_id), 'ComponentGraph: bind_port - unknown DependencyNetworkId')
      network => this%get_network(network_id)
      _ASSERT(associated(network), 'ComponentGraph: bind_port - unknown DependencyNetworkId')
      _ASSERT(.not. network%is_frozen(), 'ComponentGraph: bind_port called on a frozen network')
      _ASSERT(this%allowed_foreign_node(node_id), 'ComponentGraph: bind_port - transform NodeId is not registered anywhere in this hierarchy')
      _ASSERT(this%allowed_foreign_node(target_id), 'ComponentGraph: bind_port - target NodeId is not registered anywhere in this hierarchy')

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

      resources => get_hierarchy_resources(this%hierarchy_id)
      table => resources%graph%get_port_bindings_ptr()
      call table%bind(network_id, node_id, port_name, target_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine graph_bind_port

   function graph_get_port_binding(this, network_id, node_id, port_name) result(target_id)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      character(*), intent(in) :: port_name
      type(NodeId), pointer :: target_id

      type(HierarchyResources), pointer :: resources
      type(PortBindingTable), pointer :: table

      target_id => null()
      if (this%is_finalized()) return
      resources => get_hierarchy_resources(this%hierarchy_id)
      table => resources%graph%get_port_bindings_ptr()
      target_id => table%get_binding(network_id, node_id, port_name)
   end function graph_get_port_binding

   function graph_get_port_bindings(this, network_id, node_id) result(port_map)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: network_id
      type(NodeId), intent(in) :: node_id
      type(StateItemMemberMap) :: port_map

      type(HierarchyResources), pointer :: resources
      type(PortBindingTable), pointer :: table

      if (this%is_finalized()) return
      resources => get_hierarchy_resources(this%hierarchy_id)
      table => resources%graph%get_port_bindings_ptr()
      port_map = table%get_bindings(network_id, node_id)
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

   ! Hierarchy-wide index of built extension chains, expression evaluations
   ! and collections, so a second declaring component reuses them. Unlike
   ! the resource index, the node need not be owned by this instance.
   subroutine graph_add_reuse_entry(this, key, node_id, rc)
      class(ComponentGraph), intent(inout) :: this
      character(*), intent(in) :: key
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      type(HierarchyResources), pointer :: resources
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: add_reuse_entry called after finalize')
      _ASSERT(.not. this%is_frozen(), 'ComponentGraph: add_reuse_entry called on a frozen graph')
      _ASSERT(this%allowed_foreign_node(node_id), 'ComponentGraph: add_reuse_entry - NodeId is not registered anywhere in this hierarchy')

      resources => get_hierarchy_resources(this%hierarchy_id)
      call resources%graph%add_reuse_entry(key, node_id, _RC)

      _RETURN(_SUCCESS)
   end subroutine graph_add_reuse_entry

   function graph_get_reuse_entry(this, key, rc) result(node_id)
      class(ComponentGraph), intent(in) :: this
      character(*), intent(in) :: key
      integer, optional, intent(out) :: rc
      type(NodeId), pointer :: node_id

      type(HierarchyResources), pointer :: resources

      node_id => null()
      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: get_reuse_entry called after finalize')
      resources => get_hierarchy_resources(this%hierarchy_id)
      node_id => resources%graph%get_reuse_entry(key)

      _RETURN(_SUCCESS)
   end function graph_get_reuse_entry

   ! -- validation / lifecycle --

   ! Validates each write-back network this instance owns, on its
   ! own. The forward network spans the hierarchy and is validated once, by
   ! freeze_forward_network. A write-back target may also have a forward
   ! producer: the write-back fires only after the method runs.
   subroutine graph_validate(this, rc)
      class(ComponentGraph), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      type(DependencyNetworkIdSetIterator) :: net_iter
      type(DependencyNetworkId) :: net_id
      type(DependencyNetwork), pointer :: network
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: validate called after finalize')

      net_iter = this%own_network_ids%ftn_begin()
      do while (net_iter /= this%own_network_ids%ftn_end())
         call net_iter%next()
         net_id = net_iter%of()
         network => this%get_network(net_id)
         _ASSERT(associated(network), 'ComponentGraph: validate - owned DependencyNetworkId not found')
         call validate_network(this, network, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine graph_validate

   ! Every referenced id must be registered in this hierarchy; producer
   ! counts use the referenced nodes' own kinds, whichever graph owns them.
   subroutine validate_network(this, network, rc)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetwork), intent(in) :: network
      integer, optional, intent(out) :: rc

      type(NodeIdSet), target :: referenced_ids, producer_ids, state_item_ids
      type(NodeIdSetIterator) :: ref_iter
      type(NodeId) :: ref_id
      integer :: status

      referenced_ids = network%get_referenced_ids()
      ref_iter = referenced_ids%ftn_begin()
      do while (ref_iter /= referenced_ids%ftn_end())
         call ref_iter%next()
         ref_id = ref_iter%of()
         _ASSERT(this%allowed_foreign_node(ref_id), 'ComponentGraph: validate - referenced NodeId is not registered anywhere in this hierarchy')
      end do

      call this%classify_nodes(referenced_ids, producer_ids, state_item_ids)
      call network%validate(valid_ids=referenced_ids, producer_ids=producer_ids, &
                              state_item_ids=state_item_ids, _RC)

      _RETURN(_SUCCESS)
   end subroutine validate_network

   ! Validates first (no freeze state changes on
   ! failure), then freezes every write-back network this instance owns,
   ! then the instance itself. Idempotent if already frozen. The forward
   ! network is frozen by freeze_forward_network.
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

   ! Validates and freezes the hierarchy's forward network. Called once per
   ! hierarchy (the freeze hook), through any of its graphs. Idempotent.
   subroutine graph_freeze_forward_network(this, rc)
      class(ComponentGraph), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(DependencyNetwork), pointer :: network
      integer :: status

      _ASSERT(.not. this%is_finalized(), 'ComponentGraph: freeze_forward_network called after finalize')
      network => this%get_network(this%get_forward_network_id())
      _ASSERT(associated(network), 'ComponentGraph: freeze_forward_network - forward network not found')
      if (network%is_frozen()) then
         _RETURN(_SUCCESS)
      end if

      call validate_network(this, network, _RC)
      call network%freeze()

      _RETURN(_SUCCESS)
   end subroutine graph_freeze_forward_network

   ! Frozen and finalized are both "structure is immutable" states -
   ! finalize() may be called directly from mutable (finalize does not
   ! require a prior freeze), so this reports true for either.
   logical function graph_is_frozen(this) result(frozen)
      class(ComponentGraph), intent(in) :: this

      frozen = this%lifecycle_status /= GRAPH_LIFECYCLE_MUTABLE
   end function graph_is_frozen

   ! Explicit, fallible, never folded into a destructor.
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
   ! -- exporter (REQ-VIZ-003) and any other caller that must walk the -----
   ! -- whole graph via public API only) -----------------------------------

   ! Copy of this instance's own_node_ids record; empty once finalized like
   ! every other post-finalize accessor.
   function graph_get_node_ids(this) result(ids)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet) :: ids

      if (this%is_finalized()) return
      ids = this%own_node_ids
   end function graph_get_node_ids

   ! The forward network plus the write-back networks this instance created.
   function graph_get_network_ids(this) result(ids)
      class(ComponentGraph), target, intent(in) :: this
      type(DependencyNetworkIdSet) :: ids

      if (this%is_finalized()) return
      ids = this%own_network_ids
      call ids%insert(this%get_forward_network_id())
   end function graph_get_network_ids

   ! -- private helpers --------------------------------------------------------

   ! Classifies ids: an OperationGraphNode descendant is
   ! producer-capable; a StateItemNode is subject to the one-producer-per-
   ! state-item check.
   subroutine graph_classify_nodes(this, ids, producer_ids, state_item_ids)
      class(ComponentGraph), target, intent(in) :: this
      type(NodeIdSet), target, intent(in) :: ids
      type(NodeIdSet), intent(out) :: producer_ids
      type(NodeIdSet), intent(out) :: state_item_ids

      type(NodeIdSetIterator) :: iter
      type(NodeId) :: id
      class(GraphNode), pointer :: node

      iter = ids%ftn_begin()
      do while (iter /= ids%ftn_end())
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
            ! to the one-producer-per-state-item constraint.
         end select
      end do
   end subroutine graph_classify_nodes

end module mapl_ComponentGraph_mod
