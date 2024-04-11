module mapl_MeterNodeStack
   use mapl_MeterNodePtr


#define T MeterNodePtr
#define Vector MeterNodeStack
#define VectorIterator MeterNodeStackIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl_MeterNodeStack
