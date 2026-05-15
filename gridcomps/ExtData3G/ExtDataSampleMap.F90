module mapl3g_ExtDataSampleMap
   use mapl3g_ExtDataSample

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

end module mapl3g_ExtDataSampleMap
