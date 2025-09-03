module mapl3g_GriddedComponentDriverVector
   use mapl3g_GriddedComponentDriver

#define T GriddedComponentDriver
#define Vector GriddedComponentDriverVector
#define VectorIterator GriddedComponentDriverVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl3g_GriddedComponentDriverVector
