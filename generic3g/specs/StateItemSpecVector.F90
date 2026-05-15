module mapl3g_StateItemSpecVector
   use mapl3g_StateItemSpec, only: StateItemSpec
  
#define T StateItemSpec
#define T_deferred
#define Vector StateItemSpecVector
#define VectorIterator StateItemSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef T_deferred
#undef Vector
#undef VectorIterator
  
end module mapl3g_StateItemSpecVector
