module mapl_VirtualConnectionPtVector
   use mapl_VirtualConnectionPt

#define T VirtualConnectionPt
#define Vector VirtualConnectionPtVector
#define VectorIterator VirtualConnectionPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_VirtualConnectionPtVector
