module mapl_StateItemSpecVector_mod
   use mapl_StateItemSpec_mod, only: StateItemSpec
  
#define T StateItemSpec
#define T_deferred
#define Vector StateItemSpecVector
#define VectorIterator StateItemSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef T_deferred
#undef Vector
#undef VectorIterator
  
end module mapl_StateItemSpecVector_mod
