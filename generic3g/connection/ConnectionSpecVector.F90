module mapl3g_SimpleConnectionVector
   use mapl3g_SimpleConnection

#define T SimpleConnection
#define Vector SimpleConnectionVector
#define VectorIterator SimpleConnectionVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_SimpleConnectionVector
