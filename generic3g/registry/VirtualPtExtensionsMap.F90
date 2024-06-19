 module mapl3g_VirtualPtExtensionsMap
   use mapl3g_VirtualConnectionPt
   use mapl3g_ExtensionFamily

#define Key VirtualConnectionPt
#define Key_LT(a,b) (a < b)
#define T ExtensionFamily

#define Map VirtualPtExtensionsMap
#define MapIterator VirtualPtExtensionsMapIterator
#define Pair VirtualPtExtensionsPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl3g_VirtualPtExtensionsMap
