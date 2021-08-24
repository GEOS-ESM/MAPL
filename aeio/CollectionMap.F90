module CollectionMapMod
   use CollectionMod

#include "types/key_deferredLengthString.inc"
#define _value class(Collection)
#define _value_allocatable class(Collection)

#define _map CollectionMap
#define _iterator CollectionMapIterator
#define _pair CollectionPair
#define _alt
#include "templates/map.inc"
end module CollectionMapMod
