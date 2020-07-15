module pFIO_IntegerRequestMapMod
   use pFIO_AbstractRequestHandleMod
#include "types/key_integer.inc"
#define _value class (AbstractRequestHandle)
#define _value_allocatable
#define _alt
#define _map IntegerRequestMap
#define _iterator IntegerRequestMapIterator
#include "templates/map.inc"
end module pFIO_IntegerRequestMapMod
