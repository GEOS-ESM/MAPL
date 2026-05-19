module mapl3g_ChildSpecMap
   use mapl3g_ChildSpec

#define Key __CHARACTER_DEFERRED
#define T ChildSpec
#define OrderedMap ChildSpecMap
#define OrderedMapIterator ChildSpecMapIterator
#define Pair ChildSpecPair

#include "ordered_map/template.inc"

#undef Pair
#undef OrderedMapIterator
#undef OrderedMap
#undef T
#undef Key

end module mapl3g_ChildSpecMap
