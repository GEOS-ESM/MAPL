module mapl3g_ComponentDriverPtrVector
   use mapl3g_ComponentDriver

#define T ComponentDriverPtr
#define Vector ComponentDriverPtrVector
#define VectorIterator ComponentDriverPtrVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl3g_ComponentDriverPtrVector
