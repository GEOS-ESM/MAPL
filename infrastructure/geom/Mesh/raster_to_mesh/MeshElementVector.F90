module mapl_MeshElementVector_mod
   use mapl_MeshElement_mod

#define T MeshElement
#define Vector MeshElementVector
#define VectorIterator MeshElementVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
end module mapl_MeshElementVector_mod
