module mapl3g_ConnectionSpecVector
   use mapl3g_ConnectionSpec

#define T ConnectionSpec
#define Vector ConnectionSpecVector
#define VectorIterator ConnectionSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_ConnectionSpecVector
