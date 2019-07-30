module pFIO_IntegerIntegerMapMod
   use pFIO_ThrowMod
#include "types/key_integer.inc"
#include "types/value_integer.inc"

#define _map IntegerIntegerMap
#define _iterator IntegerIntegerMapIterator

#define _alt
#define _FTL_THROW pFIO_throw_exception

#include "templates/map.inc"

end module pFIO_IntegerIntegerMapMod
