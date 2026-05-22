module mapl_MeterNodeStack_mod
   use mapl_MeterNodePtr_mod


#define T MeterNodePtr
#define Vector MeterNodeStack
#define VectorIterator MeterNodeStackIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl_MeterNodeStack_mod
