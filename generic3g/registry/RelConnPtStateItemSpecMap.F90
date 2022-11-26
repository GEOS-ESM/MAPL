module mapl3g_RelConnPtStateItemSpecMap
   use mapl3g_RelativeConnectionPoint
   use mapl3g_AbstractStateItemSpec

#define Key RelativeConnectionPoint
#define Key_LT(a,b) (a < b)
#define T AbstractStateItemSpec
#define T_polymorphic

#define Map RelConnPtStateItemSpecMap
#define MapIterator RelConnPtStateItemSpecMapIterator
#define Pair ConnPtStateItemSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_RelConnPtStateItemSpecMap
