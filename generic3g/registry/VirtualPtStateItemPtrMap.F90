module mapl3g_VirtualPtStateItemPtrMap
   use mapl3g_VirtualConnectionPt
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecPtr

#define Key VirtualConnectionPt
#define Key_LT(a,b) (a < b)
#define T StateItemSpecPtr
#define T_polymorphic

#define Map VirtualPtStateItemPtrMap
#define MapIterator VirtualPtStateItemPtrMapIterator
#define Pair VirtualPtStateItemPtrPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_VirtualPtStateItemPtrMap
