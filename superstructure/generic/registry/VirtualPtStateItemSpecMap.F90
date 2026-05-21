module mapl_VirtualPtStateItemSpecMap
   use mapl_VirtualConnectionPt
   use mapl_StateItemSpec, only: StateItemSpec

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

end module mapl_VirtualPtStateItemSpecMap
