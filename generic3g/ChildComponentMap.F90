module mapl3g_ChildComponentMap
   use mapl3g_ChildComponent

#define Key __CHARACTER_DEFERRED
#define T ChildComponent
#define OrderedMap ChildComponentMap
#define OrderedMapIterator ChildComponentMapIterator
#define Pair ChildComponentPair

#include "ordered_map/template.inc"

#undef ChildComponentPair
#undef OrderedMapIterator
#undef OrderedMap
#undef T
#undef Key

end module mapl3g_ChildComponentMap
