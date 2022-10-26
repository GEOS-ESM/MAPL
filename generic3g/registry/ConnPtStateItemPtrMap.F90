module mapl3g_ConnPtStateItemPtrMap
   use mapl3g_ConnectionPoint
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecPtr

#define Key ConnectionPoint
#define Key_LT(a,b) (a < b)
#define T StateItemSpecPtr
#define T_polymorphic

#define Map ConnPtStateItemPtrMap
#define MapIterator ConnPtStateItemPtrMapIterator
#define Pair ConnPtStateItemPtrPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_ConnPtStateItemPtrMap
