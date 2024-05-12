module mapl3g_ActualPtComponentDriverMap
   use mapl3g_ActualConnectionPt
   use mapl3g_GriddedComponentDriver
   
#define Key ActualConnectionPt
#define Key_LT(a,b) (a < b)
#define T GriddedComponentDriver

#define Map ActualPtComponentDriverMap
#define MapIterator ActualPtComponentDriverMapIterator
#define Pair ActualPtComponentDriverMapPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key
#undef Key_LT

end module mapl3g_ActualPtComponentDriverMap
