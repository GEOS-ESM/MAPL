!------------------------------------------------------------------------------
! NodeIdGraphNodeMap: gFTL map of NodeId -> polymorphic GraphNode, the
! ComponentGraph-owned node registry (spec/07-component-graph.md REQ-CG-001,
! spec/05-identities.md REQ-ID-005/006). NodeId is directly usable as a
! gFTL map key for a polymorphic mapped value with no Box wrapper type,
! matching the pattern demonstrated by the test-only
! NodeIdMockPayloadMap_mod (graph/tests/NodeIdMockPayloadMap_mod.F90).
!------------------------------------------------------------------------------
module mapl_NodeIdGraphNodeMap_mod
   use mapl_NodeId_mod, only: NodeId, operator(<)
   use mapl_GraphNode_mod, only: GraphNode

#define Key NodeId
#define Key_LT(a,b) (a < b)
#define T GraphNode
#define T_polymorphic
#define Map NodeIdGraphNodeMap
#define MapIterator NodeIdGraphNodeMapIterator
#define Pair NodeIdGraphNodePair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key_LT
#undef Key

end module mapl_NodeIdGraphNodeMap_mod
