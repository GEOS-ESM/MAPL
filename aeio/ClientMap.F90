module AEIO_ClientMap
   use AEIO_Client

#include "types/key_deferredLengthString.inc"
#define _value class(Client)
#define _value_allocatable class(Client)

#define _map ClientMap
#define _iterator ClientMapIterator
#define _pair ClientPair
#define _alt
#include "templates/map.inc"
end module AEIO_ClientMap
