module mapl_MeshVertexVector_mod
   use mapl_MeshVertex_mod

#define T MeshVertex
#define Vector MeshVertexVector
#define VectorIterator MeshVertexVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
end module mapl_MeshVertexVector_mod
