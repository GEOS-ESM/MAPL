module MAPL_StringRouteHandleMapMod
  use ESMF

#define Key __CHARACTER_DEFERRED
#define T ESMF_RouteHandle

#define Map StringRouteHandleMap
#define MapIterator StringRouteHandleMapIterator
#define Pair StringRouteHandlePair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module MAPL_StringRouteHandleMapMod
