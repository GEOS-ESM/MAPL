module mapl3g_ComponentHandlerMap
   use mapl3g_AbstractComponentHandler
   ! Maybe should be VirtualConnectionPt instead?
#define Key __CHARACTER_DEFERRED
#define T AbstractComponentHandler
#define T_polymorphic
#define Map ComponentHandlerMap
#define MapIterator ComponentHandlerMapIterator
#define Pair ComponentHandlerPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_CouplerComponentVector
