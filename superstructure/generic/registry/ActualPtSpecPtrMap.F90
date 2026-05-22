module mapl_ActualPtSpecPtrMap_mod
   use mapl_ActualConnectionPt_mod
   use mapl_StateItemSpec_mod

#define Key ActualConnectionPt
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

end module mapl_ActualPtSpecPtrMap_mod
