module pFIO_IntegerSocketMapMod
   use pFIO_AbstractSocketMod

#include "types/key_integer.inc"
#define _value class(AbstractSocket)
#define _value_allocatable
#define _alt
#define _map IntegerSocketMap
#define _iterator IntegerSocketMapIterator

#include "templates/map.inc"
end module pFIO_IntegerSocketMapMod
