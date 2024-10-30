 module mapl3g_VirtualPtFamilyMap
   use mapl3g_VirtualConnectionPt
   use mapl3g_ExtensionFamily

#define Key VirtualConnectionPt
#define Key_LT(a,b) (a < b)
#define T ExtensionFamily

#define Map VirtualPtFamilyMap
#define MapIterator VirtualPtFamilyMapIterator
#define Pair VirtualPtFamilyPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl3g_VirtualPtFamilyMap
