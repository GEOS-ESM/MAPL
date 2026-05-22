module mapl_GriddedComponentDriverVector_mod
   use mapl_GriddedComponentDriver_mod

#define T GriddedComponentDriver
#define Vector GriddedComponentDriverVector
#define VectorIterator GriddedComponentDriverVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl_GriddedComponentDriverVector_mod
