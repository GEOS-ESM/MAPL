module mapl3g_RelConnPtStateItemPtrMap
   use mapl3g_RelativeConnectionPoint
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecPtr

#define Key RelativeConnectionPoint
#define Key_LT(a,b) (a < b)
#define T StateItemSpecPtr
#define T_polymorphic

#define Map RelConnPtStateItemPtrMap
#define MapIterator RelConnPtStateItemPtrMapIterator
#define Pair RelConnPtStateItemPtrPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_RelConnPtStateItemPtrMap
