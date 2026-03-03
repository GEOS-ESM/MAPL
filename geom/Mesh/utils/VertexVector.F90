module sf_VertexVector
   use sf_Vertex

#define T Vertex
#define Vector VertexVector
#define VectorIterator VertexVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
end module sf_VertexVector
