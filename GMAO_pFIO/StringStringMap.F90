module pFIO_StringStringMapMod
   use pFIO_ThrowMod
   use ESMF

   ! Create a map (associative array) between names and integers.

 
#include "types/key_deferredLengthString.inc"   
#include "types/value_deferredLengthString.inc"

#define _map StringStringMap
#define _iterator StringStringMapIterator

#define _alt
#define _FTL_THROW pFIO_throw_exception

#include "templates/map.inc"

end module pFIO_StringStringMapMod
