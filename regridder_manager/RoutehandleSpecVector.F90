module mapl_RoutehandleSpecVector
   use mapl_RouteHandleSpec

#define T RouteHandleSpec
#define Vector RouteHandleSpecVector
#define VectorIterator RouteHandleSpecVectorIterator
#define VectorRIterator RouteHandleSpecVectorRIterator
   
#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
#undef VectorRIterator

end module mapl_RoutehandleSpecVector
