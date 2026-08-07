module mapl_GraphNodeMap_mod
   use mapl_NodeId_mod, only: NodeId, operator(<)
   use mapl_GraphNode_mod, only: GraphNode

#define Key NodeId
#define Key_LT(a,b) (a < b)
#define T GraphNode
#define T_polymorphic
#define Map GraphNodeMap
#define MapIterator GraphNodeMapIterator
#define Pair GraphNodeMapPair
#include "map/template.inc"
#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key_LT
#undef Key

end module mapl_GraphNodeMap_mod
