module mapl_GriddedComponentDriverVector
   use mapl_GriddedComponentDriver

#define T GriddedComponentDriver
#define Vector GriddedComponentDriverVector
#define VectorIterator GriddedComponentDriverVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl_GriddedComponentDriverVector
