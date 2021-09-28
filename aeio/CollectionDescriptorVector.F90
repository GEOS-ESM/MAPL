module AEIO_CollectionDescriptorVector
   use AEIO_CollectionDescriptor

#define _type class(CollectionDescriptor)
#define _allocatable

#define _vector CollectionDescriptorVector
#define _iterator CollectionDescriptorVectorIterator
#include "templates/vector.inc"
end module AEIO_CollectionDescriptorVector
