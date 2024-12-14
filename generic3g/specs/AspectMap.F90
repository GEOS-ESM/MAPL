module mapl3g_AspectMap
   use mapl3g_StateItemAspect

#define Key __CHARACTER_DEFERRED
#define T StateItemAspect
#define T_polymorphic
#define Map AspectMap
#define MapIterator AspectMapIterator
#define Pair AspectPairIterator

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_AspectMap
