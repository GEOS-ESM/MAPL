module MAPL_StringIntegerMapMod
   use MAPL_ThrowMod
   use ESMF

   ! Create a map (associative array) between names and integers.

   
#include "types/key_deferredLengthString.inc"   
#include "types/value_integer.inc"

#define _map StringIntegerMap
#define _iterator StringIntegerMapIterator

#define _alt
#define _FTL_THROW MAPL_throw_exception

#include "templates/map.inc"
   
end module MAPL_StringIntegerMapMod
