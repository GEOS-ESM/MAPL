module mapl3g_StateItemSpecVector
   use mapl3g_StateItemSpec
  
#define T StateItemSpec
#define T_deferred
#define Vector StateItemSpecVector
#define VectorIterator StateItemSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef T_allocatable
#undef Vector
#undef VectorIterator
  
end module mapl3g_StateItemSpecVector
