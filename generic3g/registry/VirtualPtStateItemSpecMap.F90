module mapl3g_VirtualPtStateItemSpecMap
   use mapl3g_VirtualConnectionPt
   use mapl3g_AbstractStateItemSpec

#define Key VirtualConnectionPt
#define Key_LT(a,b) (a < b)
#define T AbstractStateItemSpec
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

end module mapl3g_VirtualPtStateItemSpecMap
