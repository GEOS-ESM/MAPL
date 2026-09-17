!------------------------------------------------------------------------------
! NodeId_NodeIdSet_Map: gFTL map of NodeId -> NodeIdSet, used by
! DependencyNetwork for predecessor/successor adjacency storage
! (spec/06-dependency-network.md REQ-DEP-002).
!------------------------------------------------------------------------------
module mapl_NodeId_NodeIdSet_Map_mod
   use mapl_NodeId_mod, only: NodeId, operator(<)
   use mapl_NodeIdSet_mod, only: NodeIdSet

#define Key NodeId
#define Key_LT(a,b) (a < b)
#define T NodeIdSet

#define Map NodeId_NodeIdSet_Map
#define MapIterator NodeId_NodeIdSet_MapIterator
#define Pair NodeId_NodeIdSet

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_NodeId_NodeIdSet_Map_mod
