module mapl_ExtDataCollectionMap_mod
   use mapl_ExtDataCollection_mod

#define Key __CHARACTER_DEFERRED
#define T ExtDataCollection
#define Map ExtDataCollectionMap
#define MapIterator ExtDataCollectionMapIterator
#define Pair ExtDataCollectionPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_ExtDataCollectionMap_mod
