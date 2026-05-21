module mapl_UngriddedDimVector
   use mapl_UngriddedDim

#define T UngriddedDim
#define Vector UngriddedDimVector
#define VectorIterator UngriddedDimVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_UngriddedDimVector
