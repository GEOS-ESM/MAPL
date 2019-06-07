module pFIO_IntegerMessageMapMod
   use pFIO_AbstractMessageMod
#include "types/key_integer.inc"
#define _value class(AbstractMessage)
#define _value_allocatable
#define _alt
#define _map IntegerMessageMap
#define _iterator IntegerMessageMapIterator
#include "templates/map.inc"
end module pFIO_IntegerMessageMapMod
