module mapl_GraphEdgeMap_mod
   use mapl_EdgeId_mod, only: EdgeId, operator(<)
   use mapl_GraphEdge_mod, only: GraphEdge
#define Key EdgeId
#define Key_LT(a,b) (a < b)
#define T GraphEdge
#define Map GraphEdgeMap
#define MapIterator GraphEdgeMapIterator
#define Pair GraphEdgeMapPair
#include "map/template.inc"
#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_GraphEdgeMap_mod
