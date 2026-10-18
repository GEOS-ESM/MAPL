#include "MAPL.h"

!------------------------------------------------------------------------------
! Transform: abstract computation descriptor - extends TransformBase
! (mapl_TransformBase_mod, port-declaration machinery only) adding
! exactly one thing: the deferred compute(this, inputs, outputs, rc),
! where inputs/outputs are the transform's own already-resolved, caller-
! supplied bound values (one GraphStateItem per declared port name) -
! with no graph/revision/NodeId concept of its own, matching how
! GraphStateItem (StateItemNode's payload) carries no revision concept
! either: both are the "value/behavior" half of their respective node
! kind, deliberately independent of graph plumbing, so each is usable/
! testable in complete isolation from ComponentGraph/DependencyNetwork/
! NodeRevision (openspec/changes/decouple-transform-from-graph).
!
! Split from TransformBase (openspec/changes/merge-characteristic-
! families): compute()'s own PortNameGraphStateItemMap arguments are
! exactly what gives this module a dependency chain reaching
! mapl_GraphStateItem_mod (via mapl_PortNameGraphStateItemMap_mod) -
! TransformBase.F90's own header comment has the full rationale for why
! that dependency had to be isolated to this tier specifically, not the
! common ancestor.
!
! A real Transform (Phase 3/4, in MAPL - e.g. ConvertUnitsTransform)
! extends this directly, declaring its own fixed port shape (via
! declare_input_port()/declare_output_port(), inherited from
! TransformBase) in its own constructor - port shape is intrinsic
! per-subclass domain knowledge, not something rediscovered or
! duplicated by whatever code later wires an instance into a graph.
! TransformGraphNode (see mapl_TransformGraphNode_mod) holds one
! Transform (component name "transformer") and delegates its own
! port-query methods to it, adding only what IS a graph-plumbing concern
! on top: NodeId/lifecycle (inherited via OperationGraphNode/
! BaseGraphNode) and revision/staleness bookkeeping
! (spec/11-revision-and-update.md REQ-REV-005..007).
!------------------------------------------------------------------------------
module mapl_Transform_mod
   use mapl_TransformBase_mod, only: TransformBase, operator(==), operator(/=)
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: Transform
   ! operator(==)/operator(/=) re-export: PortSpecMapIterator's own
   ! comparison operators, reached via TransformBase's own identical
   ! re-export (mapl_TransformBase_mod's own header comment) - the one
   ! legitimate exception, see mapl_GraphStateItem_mod's identical
   ! rationale.
   public :: operator(==)
   public :: operator(/=)

   type, abstract, extends(TransformBase) :: Transform
   contains
      procedure(transform_compute_interface), deferred :: compute
   end type Transform

   abstract interface
      subroutine transform_compute_interface(this, inputs, outputs, rc)
         import :: Transform, PortNameGraphStateItemMap
         class(Transform), intent(inout) :: this
         type(PortNameGraphStateItemMap), intent(in) :: inputs
         type(PortNameGraphStateItemMap), intent(in) :: outputs
         integer, intent(out) :: rc
      end subroutine transform_compute_interface
   end interface

end module mapl_Transform_mod
