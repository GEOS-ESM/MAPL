module mapl3g_ChildSpecMap
   use mapl3g_ChildSpec

#define Key __CHARACTER_DEFERRED
#define T ChildSpec
#define Map ChildSpecMap
#define MapIterator ChildSpecMapIterator
#define Pair ChildSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl3g_ChildSpecMap
