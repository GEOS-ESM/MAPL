module mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
  
#define T ActualConnectionPt
#define Vector ActualPtVector
#define VectorIterator ActualPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_ActualPtVector
