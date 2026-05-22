module mapl_VirtualPtStateItemPtrMap_mod
   use mapl_VirtualConnectionPt_mod
   use mapl_StateItemSpec_mod

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

end module mapl_VirtualPtStateItemPtrMap_mod
