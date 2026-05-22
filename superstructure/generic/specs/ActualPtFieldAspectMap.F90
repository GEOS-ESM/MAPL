module mapl_ActualPtFieldAspectMap_mod
   use mapl_FieldClassAspect_mod
   use mapl_ActualConnectionPt_mod

#define MAPL_DEBUG
   
#define Key ActualConnectionPt
#define Key_LT(a,b) (a < b)
#define T FieldClassAspect

#define Map ActualPtFieldAspectMap
#define MapIterator ActualPtFieldAspectMapIterator
#define Pair ActualPtFieldAspectPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map

#undef T
#undef Key
#undef Key_LT
end module mapl_ActualPtFieldAspectMap_mod
