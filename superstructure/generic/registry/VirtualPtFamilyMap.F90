 module mapl_VirtualPtFamilyMap_mod
   use mapl_VirtualConnectionPt_mod
   use mapl_ExtensionFamily_mod

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

end module mapl_VirtualPtFamilyMap_mod
