module FieldEntryMapMod
   use FieldEntryMod

#include "types/key_deferredLengthString.inc"
#define _value class(FieldEntry)
#define _value_allocatable class(FieldEntry)

#define _map FieldEntryMap
#define _iterator FieldEntryMapIterator
#define _pair FieldEntryPair
#define _alt
#include "templates/map.inc"
end module FieldEntryMapMod
