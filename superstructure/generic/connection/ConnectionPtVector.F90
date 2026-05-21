module mapl_ConnectionPtVector
   use mapl_ConnectionPt

#define T ConnectionPt
#define Vector ConnectionPtVector
#define VectorIterator ConnectionPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_ConnectionPtVector
