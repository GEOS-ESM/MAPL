module MAPL_StringGridMapMod
   use ESMF

   ! Create a map (associative array) between names and grid factories.

#include "types/key_deferredLengthString.inc"   
#define _value type(ESMF_Grid)

#define _map StringGridMap
#define _iterator StringGridMapIterator
#define _alt
#include "templates/map.inc"
   
end module MAPL_StringGridMapMod
