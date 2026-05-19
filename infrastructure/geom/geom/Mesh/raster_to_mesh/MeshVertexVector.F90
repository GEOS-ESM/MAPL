module mapl_MeshVertexVector
   use mapl_MeshVertex

#define T MeshVertex
#define Vector MeshVertexVector
#define VectorIterator MeshVertexVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
end module mapl_MeshVertexVector
