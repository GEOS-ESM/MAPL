module mapl_ActualPtVec_Map_mod
   use mapl_VirtualConnectionPt_mod
   use mapl_ActualPtVector_mod
   
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

end module mapl_ActualPtVec_Map_mod
