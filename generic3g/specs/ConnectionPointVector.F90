module mapl3g_ConnectionPointVector
   use mapl3g_ConnectionPoint

#define T ConnectionPoint
#define Vector ConnectionPointVector
#define VectorIterator ConnectionPointVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_ConnectionPointVector
