!------------------------------------------------------------------------------
! OperationGraphNode: abstract "something that runs" layer of the GraphNode
! hierarchy (REQ-NODE-004).
!
! Extends BaseGraphNode and adds no components of its own; it distinguishes
! "operation" nodes from StateItemNode's "value" nodes. Concrete subclasses
! are TransformGraphNode and MethodGraphNode.
!------------------------------------------------------------------------------
module mapl_OperationGraphNode_mod
   use mapl_BaseGraphNode_mod, only: BaseGraphNode
   implicit none(type, external)
   private

   public :: OperationGraphNode

   type, abstract, extends(BaseGraphNode) :: OperationGraphNode
   end type OperationGraphNode

end module mapl_OperationGraphNode_mod
