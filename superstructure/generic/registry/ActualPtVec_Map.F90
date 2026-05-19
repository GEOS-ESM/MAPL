module mapl3g_ActualPtVec_Map
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualPtVector
   
#define Key VirtualConnectionPt
#define Key_LT(a,b) (a < b)
#define T ActualPtVector

#define Map ActualPtVec_Map
#define MapIterator ActualPtVec_MapIterator
#define Pair ActualPtVec_Pair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key
#undef Key_LT

end module mapl3g_ActualPtVec_Map
