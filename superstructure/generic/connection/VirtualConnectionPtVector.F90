module mapl_VirtualConnectionPtVector_mod
   use mapl_VirtualConnectionPt_mod

#define T VirtualConnectionPt
#define Vector VirtualConnectionPtVector
#define VectorIterator VirtualConnectionPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_VirtualConnectionPtVector_mod
