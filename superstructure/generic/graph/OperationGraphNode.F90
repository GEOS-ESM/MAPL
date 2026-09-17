!------------------------------------------------------------------------------
! OperationGraphNode: abstract "something that runs" layer of the
! GraphNode hierarchy (spec/03-graph-node-hierarchy.md REQ-NODE-004).
!
! Extends BaseGraphNode, adding no components of its own in this change -
! it is a stub distinguishing "operation" nodes from StateItemNode's
! "value" nodes. TransformGraphNode/MethodGraphNode concrete subclasses
! are out of scope here (Phase 2/Phase 4, spec/10, spec/12).
!------------------------------------------------------------------------------
module mapl_OperationGraphNode_mod
   use mapl_BaseGraphNode_mod, only: BaseGraphNode
   implicit none(type, external)
   private

   public :: OperationGraphNode

   type, abstract, extends(BaseGraphNode) :: OperationGraphNode
   end type OperationGraphNode

end module mapl_OperationGraphNode_mod
