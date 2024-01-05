module mapl3g_ComponentDriverMap
   use mapl3g_ComponentDriver

#define Key __CHARACTER_DEFERRED
#define T ComponentDriver
#define OrderedMap ComponentDriverMap
#define OrderedMapIterator ComponentDriverMapIterator
#define Pair ComponentDriverPair

#include "ordered_map/template.inc"

#undef Pair
#undef OrderedMapIterator
#undef OrderedMap
#undef T
#undef Key

end module mapl3g_ComponentDriverMap
