module mapl_StateItemVector
   use mapl_StateItemSpec, only: StateItemSpec
  
#define T StateItemSpec
#define T_polymorphic
#define Vector StateItemVector
#define VectorIterator StateItemVectorIterator

#include "vector/template.inc"

#undef T
#undef T_polymorphic
#undef Vector
#undef VectorIterator
  
end module mapl_StateItemVector
