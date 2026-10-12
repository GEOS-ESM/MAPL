!------------------------------------------------------------------------------
! PortNameGraphStateItemMap: gFTL map of port name (string) -> GraphStateItem.
! Used by ComponentGraph_DemandDrivenUpdate_smod's finish_transform to
! assemble a transform's own bound input/output values (one per declared
! port name) immediately before calling TransformGraphNode%execute(), and
! passed through unchanged into Transform%compute() - see
! openspec/changes/decouple-transform-from-graph design.md Decisions.
! Mirrors PortNameRevisionMap.F90 exactly, substituting GraphStateItem for
! NodeRevision as the mapped type.
!------------------------------------------------------------------------------
module mapl_PortNameGraphStateItemMap_mod
   use mapl_GraphStateItem_mod, only: GraphStateItem

#define Key __CHARACTER_DEFERRED
#define T GraphStateItem
#define Map PortNameGraphStateItemMap
#define MapIterator PortNameGraphStateItemMapIterator
#define Pair PortNameGraphStateItemPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_PortNameGraphStateItemMap_mod
