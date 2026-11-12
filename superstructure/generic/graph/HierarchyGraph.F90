#include "MAPL.h"

!------------------------------------------------------------------------------
! HierarchyGraph: the hierarchy-wide owner of the polymorphic
! NodeId -> GraphNode map, the DependencyNetworkId -> DependencyNetwork map,
! and the one NodeIdGenerator/DependencyNetworkIdGenerator pair that issues
! fresh ids for the entire hierarchy.
!
! Exactly one instance exists per component hierarchy, held inside that
! hierarchy's HierarchyResources entry in the private registry
! (mapl_HierarchyRegistry_mod). ComponentGraph and OuterMetaComponent never
! construct or hold it; they resolve it on every access via
! get_hierarchy_resources(hierarchy_id).
!
! It also holds the one forward network (every edge that carries data toward
! a consumer, whichever component declared it), the transform port-binding
! table, and the reuse index (extension chains, expression evaluations,
! collections). PortId generation, the advertised-item resource indexes,
! ports, write-back network ownership and lifecycle
! state stay component-local in ComponentGraph.
!------------------------------------------------------------------------------
module mapl_HierarchyGraph_mod
   use mapl_NodeId_mod, only: NodeId, NodeIdGenerator
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId, DependencyNetworkIdGenerator
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_NodeIdGraphNodeMap_mod
   use mapl_DependencyNetworkIdNetworkMap_mod
   use mapl_PortBindingTable_mod, only: PortBindingTable
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: HierarchyGraph

   type :: HierarchyGraph
      private
      type(NodeIdGraphNodeMap) :: nodes
      type(DependencyNetworkIdNetworkMap) :: networks
      type(NodeIdGenerator) :: node_generator
      type(DependencyNetworkIdGenerator) :: network_generator
      type(DependencyNetworkId) :: forward_network_id
      type(PortBindingTable) :: port_bindings
      type(StateItemMemberMap) :: reuse_index
   contains
      procedure :: initialize => hgraph_initialize
      procedure :: get_forward_network_id => hgraph_get_forward_network_id
      procedure :: get_port_bindings_ptr => hgraph_get_port_bindings_ptr
      procedure :: add_reuse_entry => hgraph_add_reuse_entry
      procedure :: get_reuse_entry => hgraph_get_reuse_entry
      procedure :: next_node_id => hgraph_next_node_id
      procedure :: register_node => hgraph_register_node
      procedure :: get_node => hgraph_get_node
      procedure :: next_network_id => hgraph_next_network_id
      procedure :: register_network => hgraph_register_network
      procedure :: get_network => hgraph_get_network
   end type HierarchyGraph

contains

   ! Creates the forward network; called once, when the hierarchy is created.
   subroutine hgraph_initialize(this, rc)
      class(HierarchyGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(DependencyNetwork) :: empty_network
      integer :: status

      this%forward_network_id = this%next_network_id(_RC)
      call this%register_network(this%forward_network_id, empty_network, _RC)

      _RETURN(_SUCCESS)
   end subroutine hgraph_initialize

   function hgraph_get_forward_network_id(this) result(id)
      class(HierarchyGraph), intent(in) :: this
      type(DependencyNetworkId) :: id

      id = this%forward_network_id
   end function hgraph_get_forward_network_id

   function hgraph_get_port_bindings_ptr(this) result(table)
      class(HierarchyGraph), target, intent(in) :: this
      type(PortBindingTable), pointer :: table

      table => this%port_bindings
   end function hgraph_get_port_bindings_ptr

   subroutine hgraph_add_reuse_entry(this, key, node_id, rc)
      class(HierarchyGraph), intent(inout) :: this
      character(*), intent(in) :: key
      type(NodeId), intent(in) :: node_id
      integer, optional, intent(out) :: rc

      _ASSERT(this%reuse_index%count(key) == 0, 'HierarchyGraph: add_reuse_entry - key is already bound')
      call this%reuse_index%insert(key, node_id)

      _RETURN(_SUCCESS)
   end subroutine hgraph_add_reuse_entry

   function hgraph_get_reuse_entry(this, key) result(node_id)
      class(HierarchyGraph), target, intent(in) :: this
      character(*), intent(in) :: key
      type(NodeId), pointer :: node_id

      node_id => this%reuse_index%at(key)
   end function hgraph_get_reuse_entry

   ! One counter for the whole hierarchy: two components can only be issued the
   ! same NodeId if two generators exist, and this type guarantees one.
   function hgraph_next_node_id(this, rc) result(id)
      class(HierarchyGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(NodeId) :: id

      integer :: status

      id = this%node_generator%next(status)
      _ASSERT(status == 0, 'HierarchyGraph: NodeId generator exhausted')

      _RETURN(_SUCCESS)
   end function hgraph_next_node_id

   ! No second node silently replaces one registered under the same NodeId,
   ! anywhere in the hierarchy.
   subroutine hgraph_register_node(this, node, rc)
      class(HierarchyGraph), intent(inout) :: this
      class(GraphNode), target, intent(in) :: node
      integer, optional, intent(out) :: rc

      type(NodeId) :: id

      id = node%get_node_id()
      _ASSERT(id%is_valid(), 'HierarchyGraph: register_node requires a NodeId obtained via next_node_id()')
      _ASSERT(this%nodes%count(id) == 0, 'HierarchyGraph: register_node - NodeId is already registered')

      call this%nodes%insert(id, node)

      _RETURN(_SUCCESS)
   end subroutine hgraph_register_node

   ! Any node in the hierarchy is retrievable by NodeId, with no ownership
   ! check; ComponentGraph's owns_node() is the ownership-scoped query.
   function hgraph_get_node(this, id) result(node)
      class(HierarchyGraph), target, intent(in) :: this
      type(NodeId), intent(in) :: id
      class(GraphNode), pointer :: node

      node => this%nodes%at(id)
   end function hgraph_get_node

   function hgraph_next_network_id(this, rc) result(id)
      class(HierarchyGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(DependencyNetworkId) :: id

      integer :: status

      id = this%network_generator%next(status)
      _ASSERT(status == 0, 'HierarchyGraph: DependencyNetworkId generator exhausted')

      _RETURN(_SUCCESS)
   end function hgraph_next_network_id

   subroutine hgraph_register_network(this, id, network, rc)
      class(HierarchyGraph), intent(inout) :: this
      type(DependencyNetworkId), intent(in) :: id
      type(DependencyNetwork), intent(in) :: network
      integer, optional, intent(out) :: rc

      _ASSERT(id%is_valid(), 'HierarchyGraph: register_network requires a DependencyNetworkId obtained via next_network_id()')
      _ASSERT(this%networks%count(id) == 0, 'HierarchyGraph: register_network - DependencyNetworkId is already registered')

      call this%networks%insert(id, network)

      _RETURN(_SUCCESS)
   end subroutine hgraph_register_network

   function hgraph_get_network(this, id) result(network)
      class(HierarchyGraph), target, intent(in) :: this
      type(DependencyNetworkId), intent(in) :: id
      type(DependencyNetwork), pointer :: network

      network => this%networks%at(id)
   end function hgraph_get_network

end module mapl_HierarchyGraph_mod
