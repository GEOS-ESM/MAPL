module mapl_ConnectionPtVector_mod
   use mapl_ConnectionPt_mod

#define T ConnectionPt
#define Vector ConnectionPtVector
#define VectorIterator ConnectionPtVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_ConnectionPtVector_mod
