module mapl_VirtualPtStateItemSpecMap_mod
   use mapl_VirtualConnectionPt_mod
   use mapl_StateItemSpec_mod, only: StateItemSpec

#define Key VirtualConnectionPt
#define Key_LT(a,b) (a < b)
#define T StateItemSpec
#define T_polymorphic

#define Map VirtualPtStateItemSpecMap
#define MapIterator VirtualPtStateItemSpecMapIterator
#define Pair VirtualPtStateItemSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl_VirtualPtStateItemSpecMap_mod
