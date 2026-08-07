module mapl_NodeIdSet_mod
   use mapl_NodeId_mod, only: NodeId, operator(<)
#define T NodeId
#define T_LT(a,b) (a < b)
#define Set NodeIdSet

#include "set/template.inc"

#undef Set
#undef T_LT
#undef T
end module mapl_NodeIdSet_mod
