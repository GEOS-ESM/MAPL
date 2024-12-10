module mapl3g_VirtualConnectionPtVector
   use mapl3g_VirtualConnectionPt

#define T VirtualConnectionPt
#define Vector VirtualConnectionPtVector
#define VectorIterator VirtualConnectionPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl3g_VirtualConnectionPtVector
