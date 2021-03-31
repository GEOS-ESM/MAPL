module mapl_RegridderSpecRouteHandleMap
   use mapl_RegridderSpec
   use ESMF
   
#define _key type(RegridderSpec)
#define _key_less_than_defined   
#define _value type (ESMF_RouteHandle)

#define _map RegridderSpecRouteHandleMap
#define _iterator RegridderSpecRouteHandleMapIterator
#define _alt
#include "templates/map.inc"
   
end module mapl_RegridderSpecRouteHandleMap

