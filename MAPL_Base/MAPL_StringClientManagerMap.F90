module MAPL_StringClientManagerMapMod
  use pFIO_ClientManagerMod

#include "types/key_deferredLengthString.inc"
#define _value type (ClientManager)

#define _map StringClientManagerMap
#define _iterator StringClientManagerMapIterator
#include "templates/map.inc"

end module MAPL_StringClientManagerMapMod
