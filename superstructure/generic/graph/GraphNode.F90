!------------------------------------------------------------------------------
! GraphNode: abstract, data-free interface at the root of the GraphNode
! hierarchy (spec/03-graph-node-hierarchy.md REQ-NODE-001).
!
! Declares identity/dispatch contracts only - no stored data component of
! any kind. BaseGraphNode is the first descendant to add state (NodeId,
! lifecycle status).
!------------------------------------------------------------------------------
module mapl_GraphNode_mod
   use mapl_NodeId_mod, only: NodeId
   implicit none(type, external)
   private

   public :: GraphNode

   type, abstract :: GraphNode
   contains
      procedure(get_node_id_interface), deferred :: get_node_id
   end type GraphNode

   abstract interface
      function get_node_id_interface(this) result(id)
         import :: GraphNode, NodeId
         class(GraphNode), intent(in) :: this
         type(NodeId) :: id
      end function get_node_id_interface
   end interface

end module mapl_GraphNode_mod
