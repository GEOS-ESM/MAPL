module mapl3g_ConnectionPtVector
   use mapl3g_ConnectionPt

#define T ConnectionPt
#define Vector ConnectionPtVector
#define VectorIterator ConnectionPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_ConnectionPtVector
