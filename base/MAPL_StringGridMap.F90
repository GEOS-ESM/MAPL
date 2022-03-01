module MAPL_StringGridMapMod
   use ESMF

   ! Create a map (associative array) between names and grid factories.

#define Key __CHARACTER_DEFERRED
#define T ESMF_Grid
#define Map StringGridMap
#define MapIterator StringGridMapIterator
#define Pair StringGridPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module MAPL_StringGridMapMod
