module mapl3g_UngriddedDimVector
   use mapl3g_UngriddedDim

#define T UngriddedDim
#define Vector UngriddedDimVector
#define VectorIterator UngriddedDimVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_UngriddedDimVector
