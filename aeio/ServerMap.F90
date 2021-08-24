module AEIO_ServerMap
   use AEIO_Server

#include "types/key_deferredLengthString.inc"
#define _value class(Server)
#define _value_allocatable class(Server)

#define _map ServerMap
#define _iterator ServerMapIterator
#define _pair ServerPair
#define _alt
#include "templates/map.inc"
end module AEIO_ServerMap
