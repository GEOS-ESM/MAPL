#include "MAPL.h"

!------------------------------------------------------------------------------
! BaseGraphNode: the common-property layer of the GraphNode hierarchy
! (REQ-NODE-002/002a).
!
! Contains exactly the properties common to every graph node: a
! lifecycle-status value and its own NodeId. No adjacency (predecessors/
! successors) is stored here or anywhere in this hierarchy; that is
! DependencyNetwork's job.
!
! NodeId is set once, at construction, via the BaseGraphNode(id) structure
! constructor. There is no public mutator: the component is private and only
! the constructor and get_node_id() are public (REQ-NODE-002a: "set once ...
! never mutated afterward").
!------------------------------------------------------------------------------
module mapl_BaseGraphNode_mod
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_NodeId_mod, only: NodeId, INVALID_NodeId
   implicit none(type, external)
   private

   public :: BaseGraphNode

   ! Placeholder lifecycle vocabulary: the spec requires only that a lifecycle-
   ! status property exist, not what values or transitions mean. CREATED/ACTIVE/
   ! RETIRED are bare-minimum stand-ins; nothing branches on their values.
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
