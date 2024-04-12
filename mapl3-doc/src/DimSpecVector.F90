module mapl3g_DimSpecVector
   use mapl3g_UngriddedDimSpec

#define T UngriddedDimSpec
#define Vector DimSpecVector
#define VectorIterator DimSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_DimSpecVector
