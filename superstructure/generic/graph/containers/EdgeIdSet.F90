module mapl_EdgeIdSet_mod
   use mapl_EdgeId_mod, only: EdgeId, operator(<)
#define T EdgeId
#define T_LT(a,b) (a < b)
#define Set EdgeIdSet
#define SetIterator EdgeIdSetIterator

#include "set/template.inc"

#undef SetIterator
#undef Set
#undef T_LT
#undef T
end module mapl_EdgeIdSet_mod
