module mapl3g_RegistryPtrMap
   use mapl3g_RegistryPtr
   
#define Key __CHARACTER_DEFERRED
#define T RegistryPtr

#define Map RegistryPtrMap
#define MapIterator RegistryPtrMapIterator
#define Pair RegistryPtrPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl3g_RegistryPtrMap
