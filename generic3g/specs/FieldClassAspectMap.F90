module mapl3g_ActualPtFieldAspectMap
   use mapl3g_FieldClassAspect
   use mapl3g_ActualConnectionPt

#define MAPL_DEBUG

#define USE_ALT_SET
#define Key ActualConnectionPt
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

end module mapl3g_FieldClassAspectMap
