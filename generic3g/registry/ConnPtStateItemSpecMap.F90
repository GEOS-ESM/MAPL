module mapl3g_ConnPtStateItemSpecMap
   use mapl3g_ConnectionPoint
   use mapl3g_AbstractStateItemSpec

#define Key ConnectionPoint
#define Key_LT(a,b) (a < b)
#define T AbstractStateItemSpec
#define T_polymorphic

#define Map ConnPtStateItemSpecMap
#define MapIterator ConnPtStateItemSpecMapIterator
#define Pair ConnPtStateItemSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_ConnPtStateItemSpecMap
