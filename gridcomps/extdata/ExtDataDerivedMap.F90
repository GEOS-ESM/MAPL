module mapl_ExtDataDerivedMap_mod
   use mapl_ExtDataDerived_mod

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

end module mapl_ExtDataDerivedMap_mod
