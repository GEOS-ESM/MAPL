#include "MAPL.h"

!------------------------------------------------------------------------------
! HierarchyGraph: the hierarchy-wide owner of the polymorphic
! NodeId -> GraphNode map, the DependencyNetworkId -> DependencyNetwork
! map, and the one NodeIdGenerator/DependencyNetworkIdGenerator pair that
! issues fresh ids for the entire hierarchy
! (openspec/changes/hierarchy-wide-component-graph, design.md D1).
!
! Exactly one instance of this type exists per component hierarchy, held
! inside that hierarchy's own HierarchyResources entry in the private
! registry (mapl_HierarchyRegistry_mod) - never constructed or held
! directly by ComponentGraph or OuterMetaComponent, which instead resolve
! it, fresh, on every access, via get_hierarchy_resources(hierarchy_id).
!
! Deliberately holds ONLY the two map/generator pairs that move per
! design.md's scope (proposal.md "What Changes"); PortId generation,
! resource indexes, ports, child-port bindings, and lifecycle state all
! remain exactly as component-local as before, owned by ComponentGraph
! itself, not here.
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

   ! One counter for the entire hierarchy (design.md D1): the only way two
   ! different components could ever be issued the same NodeId is if two
   ! different generators existed - this type guarantees exactly one does.
   function hgraph_next_node_id(this, rc) result(id)
      class(HierarchyGraph), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(NodeId) :: id

      integer :: status

      id = this%node_generator%next(status)
      _ASSERT(status == 0, 'HierarchyGraph: NodeId generator exhausted')

      _RETURN(_SUCCESS)
   end function hgraph_next_node_id

   ! No second node silently replaces one already registered under the same
   ! NodeId - this invariant is now hierarchy-wide (any component's node),
   ! not merely per-ComponentGraph.
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

   ! Any node anywhere in the hierarchy is retrievable by its NodeId, by
   ! design (graph/component-hierarchy's "Two components' nodes live in one
   ! shared store" scenario) - no ownership check here; ComponentGraph's own
   ! owns_node() is the ownership-scoped query.
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
