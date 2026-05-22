module mapl_ComponentDriverPtrVector_mod
   use mapl_ComponentDriver_mod

#define T ComponentDriverPtr
#define Vector ComponentDriverPtrVector
#define VectorIterator ComponentDriverPtrVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl_ComponentDriverPtrVector_mod
