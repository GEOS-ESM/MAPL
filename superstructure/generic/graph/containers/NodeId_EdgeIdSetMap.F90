module mapl_NodeId_EdgeIdSetMap_mod
   use mapl_NodeId_mod, only: NodeId, operator(<)
   use mapl_EdgeIdSet_mod, only: EdgeIdSet

#define Key NodeId
#define Key_LT(a,b) (a < b)
#define T EdgeIdSet
#define Map NodeId_EdgeIdSetMap
#define MapIterator NodeId_EdgeIdSetMapIterator
#define Pair NodeId_EdgeIdSetMapPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_NodeId_EdgeIdSetMap_mod
   
