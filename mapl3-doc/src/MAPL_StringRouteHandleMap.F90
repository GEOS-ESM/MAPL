module MAPL_StringRouteHandleMapMod
  use ESMF

#include "types/key_deferredLengthString.inc"
#define _value type (ESMF_RouteHandle)

#define _map StringRouteHandleMap
#define _iterator StringRouteHandleMapIterator
#include "templates/map.inc"

end module MAPL_StringRouteHandleMapMod
