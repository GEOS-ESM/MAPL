module mapl_ComponentDriverPtrVector
   use mapl_ComponentDriver

#define T ComponentDriverPtr
#define Vector ComponentDriverPtrVector
#define VectorIterator ComponentDriverPtrVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl_ComponentDriverPtrVector
