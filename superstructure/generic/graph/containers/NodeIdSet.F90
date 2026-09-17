!------------------------------------------------------------------------------
! NodeIdSet: gFTL set of NodeId, used by DependencyNetwork for predecessor/
! successor adjacency (spec/06-dependency-network.md REQ-DEP-002).
!------------------------------------------------------------------------------
module mapl_NodeIdSet_mod
   use mapl_NodeId_mod, only: NodeId, operator(<)

#define T NodeId
#define T_LT(a,b) (a < b)
#define Set NodeIdSet
#define SetIterator NodeIdSetIterator

#include "set/template.inc"

#undef SetIterator
#undef Set
#undef T_LT
#undef T

end module mapl_NodeIdSet_mod
