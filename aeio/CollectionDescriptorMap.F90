module AEIO_CollectionDescriptorMap
   use AEIO_CollectionDescriptor

#include "types/key_deferredLengthString.inc"
#define _value class(CollectionDescriptor)
#define _value_allocatable class(CollectionDescriptor)

#define _map CollectionDescriptorMap
#define _iterator CollectionDescriptorMapIterator
#define _pair CollectionDescriptorPair
#define _alt
#include "templates/map.inc"
end module AEIO_CollectionDescriptorMap
