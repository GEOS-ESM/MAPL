module mapl_ExtDataSampleMap_mod
   use mapl_ExtDataSample_mod

#define Key __CHARACTER_DEFERRED
#define T ExtDataSample
#define Map ExtDataSampleMap
#define MapIterator ExtDataSampleMapIterator
#define Pair ExtDataSamplePair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_ExtDataSampleMap_mod
