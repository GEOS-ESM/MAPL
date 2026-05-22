module mapl_UngriddedDimVector_mod
   use mapl_UngriddedDim_mod

#define T UngriddedDim
#define Vector UngriddedDimVector
#define VectorIterator UngriddedDimVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_UngriddedDimVector_mod
