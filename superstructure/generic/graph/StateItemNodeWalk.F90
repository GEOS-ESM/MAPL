#include "MAPL.h"

! Visit each StateItemNode a ComponentGraph itself registered. Depends only
! on graph-layer modules, never on OuterMetaComponent or StateRegistry.
module mapl_StateItemNodeWalk_mod
   use mapl_ComponentGraph_mod, only: ComponentGraph
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_StateItemNode_mod, only: StateItemNode
   use mapl_NodeIdSet_mod, only: NodeIdSet, NodeIdSetIterator, operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: I_state_item_node_op
   public :: for_each_state_item_node

   ! Per-node callback for for_each_state_item_node(). Callers pass an
   ! internal procedure that host-associates its context.
   abstract interface
      subroutine I_state_item_node_op(node, rc)
         import StateItemNode
         class(StateItemNode), target, intent(inout) :: node
         integer, optional, intent(out) :: rc
      end subroutine I_state_item_node_op
   end interface

contains

   ! Calls op once per StateItemNode among graph%get_node_ids() (this
   ! instance's own nodes, in NodeIdSet order, not the hierarchy's), silently
   ! skipping an id with no node or a node of another class. A failing op
   ! stops the walk and its status is returned; an op that must not stop it
   ! reports success itself.
   subroutine for_each_state_item_node(graph, op, rc)
      class(ComponentGraph), target, intent(in) :: graph
      procedure(I_state_item_node_op) :: op
      integer, optional, intent(out) :: rc

      integer :: status
      type(NodeIdSet), target :: node_ids
      type(NodeIdSetIterator) :: iter
      class(GraphNode), pointer :: node

      node_ids = graph%get_node_ids()
      associate (e => node_ids%ftn_end())
         iter = node_ids%ftn_begin()
         do while (iter /= e)
            call iter%next()
            node => graph%get_node(iter%of())
            if (.not. associated(node)) cycle
            select type (node)
            class is (StateItemNode)
               call op(node, _RC)
            class default
               cycle
            end select
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine for_each_state_item_node

end module mapl_StateItemNodeWalk_mod
