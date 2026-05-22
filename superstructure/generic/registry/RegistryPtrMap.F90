module mapl_RegistryPtrMap_mod
   use mapl_RegistryPtr_mod
   
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

end module mapl_RegistryPtrMap_mod
