module mapl_ActualPtVector
   use mapl_ActualConnectionPt
  
#define T ActualConnectionPt
#define Vector ActualPtVector
#define VectorIterator ActualPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_ActualPtVector
