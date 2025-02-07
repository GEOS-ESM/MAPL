module mapl3g_ActualPtFieldAspectMap
   use mapl3g_FieldClassAspect
   use mapl3g_ActualConnectionPt

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
end module mapl3g_ActualPtFieldAspectMap
