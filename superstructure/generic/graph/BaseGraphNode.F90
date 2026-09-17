#include "MAPL.h"

!------------------------------------------------------------------------------
! BaseGraphNode: the common-property layer of the GraphNode hierarchy
! (spec/03-graph-node-hierarchy.md REQ-NODE-002/002a).
!
! Contains exactly the properties common to every graph node: a
! lifecycle-status value and its own NodeId. No adjacency (predecessors/
! successors) is stored here or anywhere in this hierarchy - adjacency is
! DependencyNetwork's job (spec/06-dependency-network.md).
!
! NodeId is set once, at construction, via the BaseGraphNode(id) structure
! constructor. There is no public mutator: encapsulation is enforced by
! keeping the component private and offering only the constructor and the
! get_node_id() accessor as public entry points, matching REQ-NODE-002a's
! "set once ... never mutated afterward."
!------------------------------------------------------------------------------
module mapl_BaseGraphNode_mod
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_NodeId_mod, only: NodeId, INVALID_NodeId
   implicit none(type, external)
   private

   public :: BaseGraphNode

   ! Placeholder lifecycle vocabulary. The spec (spec/03) only requires
   ! that BaseGraphNode carry *a* lifecycle-status property - it does
   ! not define what values or transitions are meaningful (that is
   ! owned by whatever later change gives lifecycle status real
   ! semantics, e.g. ComponentGraph's mutable/frozen state, spec/07
   ! §7.3, or a node-level analogue). CREATED/ACTIVE/RETIRED here are
   ! bare-minimum stand-ins so the component exists and is settable;
   ! nothing in this change branches on their values. Likely to become
   ! its own small flag type (matching MAPL_StateItem_Flag's style)
   ! once real semantics are defined - not done now to avoid inventing
   ! semantics the spec doesn't ask for.
   integer, parameter, public :: NODE_LIFECYCLE_CREATED = 1
   integer, parameter, public :: NODE_LIFECYCLE_ACTIVE = 2
   integer, parameter, public :: NODE_LIFECYCLE_RETIRED = 3

   type, extends(GraphNode) :: BaseGraphNode
      private
      type(NodeId) :: node_id = INVALID_NodeId
      integer :: lifecycle_status = NODE_LIFECYCLE_CREATED
   contains
      procedure :: get_node_id => base_get_node_id
      procedure :: get_lifecycle_status => base_get_lifecycle_status
      procedure :: set_lifecycle_status => base_set_lifecycle_status
   end type BaseGraphNode

   interface BaseGraphNode
      module procedure new_BaseGraphNode
   end interface BaseGraphNode

contains

   function new_BaseGraphNode(id) result(node)
      type(NodeId), intent(in) :: id
      type(BaseGraphNode) :: node

      node%node_id = id
      node%lifecycle_status = NODE_LIFECYCLE_CREATED
   end function new_BaseGraphNode

   function base_get_node_id(this) result(id)
      class(BaseGraphNode), intent(in) :: this
      type(NodeId) :: id

      id = this%node_id
   end function base_get_node_id

   integer function base_get_lifecycle_status(this) result(status)
      class(BaseGraphNode), intent(in) :: this

      status = this%lifecycle_status
   end function base_get_lifecycle_status

   subroutine base_set_lifecycle_status(this, status)
      class(BaseGraphNode), intent(inout) :: this
      integer, intent(in) :: status

      this%lifecycle_status = status
   end subroutine base_set_lifecycle_status

end module mapl_BaseGraphNode_mod
