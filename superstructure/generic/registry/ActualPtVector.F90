module mapl_ActualPtVector_mod
   use mapl_ActualConnectionPt_mod
  
#define T ActualConnectionPt
#define Vector ActualPtVector
#define VectorIterator ActualPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_ActualPtVector_mod
