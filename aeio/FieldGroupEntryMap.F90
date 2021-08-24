module FieldGroupEntryMapMod
   use FieldGroupEntryMod

#include "types/key_deferredLengthString.inc"
#define _value class(FieldGroupEntry)
#define _value_allocatable class(FieldGroupEntry)

#define _map FieldGroupEntryMap
#define _iterator FieldGroupEntryMapIterator
#define _pair FieldGroupEntryPair
#define _alt
#include "templates/map.inc"
end module FieldGroupEntryMapMod
