module GroupMapMod
   use GroupMod

#include "types/key_deferredLengthString.inc"
#define _value class(Group)
#define _value_allocatable class(Group)

#define _map GroupMap
#define _iterator GroupMapIterator
#define _pair GroupPair
#define _alt
#include "templates/map.inc"
end module GroupMapMod
