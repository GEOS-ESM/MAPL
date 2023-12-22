module mapl3g_ComponentHandlerMap
   use mapl3g_ComponentHandler

#define Key __CHARACTER_DEFERRED
#define T ComponentHandler
#define OrderedMap ComponentHandlerMap
#define OrderedMapIterator ComponentHandlerMapIterator
#define Pair ComponentHandlerPair

#include "ordered_map/template.inc"

#undef Pair
#undef OrderedMapIterator
#undef OrderedMap
#undef T
#undef Key

end module mapl3g_ComponentHandlerMap
