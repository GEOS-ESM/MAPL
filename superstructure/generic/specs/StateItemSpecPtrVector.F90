module mapl_StateItemSpecPtrVector_mod
   use mapl_StateItemSpec_mod
  
#define T StateItemSpecPtr
#define Vector StateItemSpecPtrVector
#define VectorIterator StateItemSpecPtrVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_StateItemSpecPtrVector_mod
