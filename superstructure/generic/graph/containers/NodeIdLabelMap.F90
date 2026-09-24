!------------------------------------------------------------------------------
! NodeIdLabelMap: gFTL map of NodeId -> NodeLabel (a label string plus an
! is_proxy flag), built by GraphBuilder's enrichment procedure and
! consumed by GraphExport.F90's optional label-lookup parameter
! (visualization-enrichment-layer change). Mirrors the existing
! NodeId-keyed containers in this directory (NodeIdGraphNodeMap,
! NodeId_NodeIdSet_Map); NodeLabel is a concrete (non-polymorphic)
! Data-only type, matching the PortSpecMap/PortNameRevisionMap pattern
! rather than the T_polymorphic GraphNode pattern.
!------------------------------------------------------------------------------
module mapl_NodeIdLabelMap_mod
   use mapl_NodeId_mod, only: NodeId, operator(<)
   use mapl_NodeLabel_mod, only: NodeLabel

#define Key NodeId
#define Key_LT(a,b) (a < b)
#define T NodeLabel
#define Map NodeIdLabelMap
#define MapIterator NodeIdLabelMapIterator
#define Pair NodeIdLabelPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_NodeIdLabelMap_mod
