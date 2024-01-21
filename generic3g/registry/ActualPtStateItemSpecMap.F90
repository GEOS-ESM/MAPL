module mapl3g_ActualPtStateItemSpecMap
   use mapl3g_ActualConnectionPt
   use mapl3g_StateItemSpec

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

end module mapl3g_ActualPtStateItemSpecMap
