module mapl_MeshElementVector
   use mapl_MeshElement

#define T MeshElement
#define Vector MeshElementVector
#define VectorIterator MeshElementVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
end module mapl_MeshElementVector
