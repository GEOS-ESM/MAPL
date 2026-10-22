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
! It holds only those two map/generator pairs. PortId generation, resource
! indexes, ports, child-port bindings, and lifecycle state stay
! component-local in ComponentGraph.
!------------------------------------------------------------------------------
module mapl_HierarchyGraph_mod
   use mapl_NodeId_mod, only: NodeId, NodeIdGenerator
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId, DependencyNetworkIdGenerator
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_DependencyNetwork_mod, only: DependencyNetwork
   use mapl_NodeIdGraphNodeMap_mod
   use mapl_DependencyNetworkIdNetworkMap_mod
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
   contains
      procedure :: next_node_id => hgraph_next_node_id
      procedure :: register_node => hgraph_register_node
      procedure :: get_node => hgraph_get_node
      procedure :: next_network_id => hgraph_next_network_id
      procedure :: register_network => hgraph_register_network
      procedure :: get_network => hgraph_get_network
   end type HierarchyGraph

contains

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
