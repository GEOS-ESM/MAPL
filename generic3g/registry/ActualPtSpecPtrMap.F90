module mapl3g_ActualPtSpecPtrMap
   use mapl3g_newActualConnectionPt
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecPtr

#define Key newActualConnectionPt
#define Key_LT(a,b) (a < b)
#define T StateItemSpecPtr
#define T_polymorphic

#define Map ActualPtSpecPtrMap
#define MapIterator ActualPtSpecPtrMapIterator
#define Pair ActualPtSpecPtrPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_ActualPtSpecPtrMap
