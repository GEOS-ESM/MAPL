module mapl_ActualPtStateItemSpecMap_mod
   use mapl_ActualConnectionPt_mod
   use mapl_StateItemSpec_mod, only: StateItemSpec

#define Key ActualConnectionPt
#define Key_LT(a,b) (a < b)
#define T StateItemSpec
#define T_polymorphic

#define Map ActualPtStateItemSpecMap
#define MapIterator ActualPtStateItemSpecMapIterator
#define Pair ActualPtStateItemSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl_ActualPtStateItemSpecMap_mod
