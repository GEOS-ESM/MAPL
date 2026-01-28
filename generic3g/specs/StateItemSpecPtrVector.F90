module mapl3g_StateItemSpecPtrVector
   use mapl3g_StateItemSpec
  
#define T StateItemSpecPtr
#define Vector StateItemSpecPtrVector
#define VectorIterator StateItemSpecPtrVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_StateItemSpecPtrVector
