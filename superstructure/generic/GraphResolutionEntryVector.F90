module mapl_GraphResolutionEntryVector_mod
   use mapl_GraphResolutionEntry_mod

#define T GraphResolutionEntry
#define Vector GraphResolutionEntryVector
#define VectorIterator GraphResolutionEntryVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_GraphResolutionEntryVector_mod
