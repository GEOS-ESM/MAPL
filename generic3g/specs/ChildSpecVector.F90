! TBD - replace with MAP on next iteration
module mapl3g_ChildSpecVector
   use mapl3g_ChildSpec

#define T ChildSpec
#define Vector ChildSpecVector
#define VectorIterator ChildSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_ChildSpecVector
