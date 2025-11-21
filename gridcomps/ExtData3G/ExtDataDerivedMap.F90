module mapl3g_ExtDataDerivedMap
   use mapl3g_ExtDataDerived

#define Key __CHARACTER_DEFERRED
#define T ExtDataDerived
#define Map ExtDataDerivedMap
#define MapIterator ExtDataDerivedMapIterator
#define Pair ExtDataDerivedPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl3g_ExtDataDerivedMap
