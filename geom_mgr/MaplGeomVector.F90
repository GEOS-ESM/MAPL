module mapl_MaplGeomVector
   use mapl_MaplGeom

#define T MaplGeom
#define Vector MaplGeomVector
#define VectorIterator MaplGeomVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
   
end module mapl_MaplGeomVector
