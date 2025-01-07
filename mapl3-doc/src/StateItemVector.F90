module mapl3g_StateItemVector
   use mapl3g_StateItemSpec
  
#define T StateItemSpec
#define T_polymorphic
#define Vector StateItemVector
#define VectorIterator StateItemVectorIterator

#include "vector/template.inc"

#undef T
#undef T_polymorphic
#undef Vector
#undef VectorIterator
  
end module mapl3g_StateItemVector
