module mapl_VirtualPtStateItemPtrMap
   use mapl_VirtualConnectionPt
   use mapl_StateItemSpec

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

end module mapl_VirtualPtStateItemPtrMap
