module mapl_StateItemSpecPtrVector
   use mapl_StateItemSpec
  
#define T StateItemSpecPtr
#define Vector StateItemSpecPtrVector
#define VectorIterator StateItemSpecPtrVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_StateItemSpecPtrVector
