#include "MAPL.h"

!------------------------------------------------------------------------------
! StateItemNode: concrete GraphNode holding exactly one GraphStateItem payload
! and one NodeRevision (spec/03-graph-node-hierarchy.md REQ-NODE-003a,
! spec/04-graph-value-hierarchy.md REQ-SI-005), in addition to the NodeId
! and lifecycle-status properties inherited from BaseGraphNode. No
! adjacency.
!------------------------------------------------------------------------------
module mapl_StateItemNode_mod
   use mapl_BaseGraphNode_mod, only: BaseGraphNode
   use mapl_NodeId_mod, only: NodeId
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_NodeRevision_mod, only: NodeRevision
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: StateItemNode

   type, extends(BaseGraphNode) :: StateItemNode
      private
      type(GraphStateItem) :: payload
      type(NodeRevision) :: revision
   contains
      procedure :: get_payload => stateitemnode_get_payload
      procedure :: set_payload => stateitemnode_set_payload
      procedure :: get_revision => stateitemnode_get_revision
      procedure :: set_revision => stateitemnode_set_revision
      procedure :: advance_revision => stateitemnode_advance_revision
   end type StateItemNode

   interface StateItemNode
      module procedure new_StateItemNode
   end interface StateItemNode

contains

   function new_StateItemNode(id, payload, revision) result(node)
      type(NodeId), intent(in) :: id
      type(GraphStateItem), intent(in) :: payload
      type(NodeRevision), intent(in) :: revision
      type(StateItemNode) :: node

      node%BaseGraphNode = BaseGraphNode(id)
      node%payload = payload
      node%revision = revision
   end function new_StateItemNode

   function stateitemnode_get_payload(this) result(payload)
      class(StateItemNode), intent(in) :: this
      type(GraphStateItem) :: payload

      payload = this%payload
   end function stateitemnode_get_payload

   subroutine stateitemnode_set_payload(this, payload)
      class(StateItemNode), intent(inout) :: this
      type(GraphStateItem), intent(in) :: payload

      this%payload = payload
   end subroutine stateitemnode_set_payload

   function stateitemnode_get_revision(this) result(revision)
      class(StateItemNode), intent(in) :: this
      type(NodeRevision) :: revision

      revision = this%revision
   end function stateitemnode_get_revision

   ! Raw revision assignment - retained from Phase 1 for construction-time
   ! setup (existing tests/callers depend on it). REQ-REV-002/003's
   ! "only advance, never assign an arbitrary value to mean 'changed'"
   ! path is advance_revision() below; that is what the demand-driven
   ! update engine (spec/11-revision-and-update.md) uses.
   subroutine stateitemnode_set_revision(this, revision)
      class(StateItemNode), intent(inout) :: this
      type(NodeRevision), intent(in) :: revision

      this%revision = revision
   end subroutine stateitemnode_set_revision

   ! REQ-REV-002/003: the only mutation path that means "this node's
   ! logical value just changed" - delegates to NodeRevision%advance(),
   ! never assigns a caller-fabricated value.
   subroutine stateitemnode_advance_revision(this, rc)
      class(StateItemNode), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%revision%advance(_RC)

      _RETURN(_SUCCESS)
   end subroutine stateitemnode_advance_revision

end module mapl_StateItemNode_mod
