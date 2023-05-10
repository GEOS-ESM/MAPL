module mapl_RoutehandleVector
   use esmf, only: ESMF_Routehandle

#define T ESMF_Routehandle
#define Vector RoutehandleVector
#define VectorIterator RoutehandleVectorIterator
#define VectorRIterator RoutehandleVectorRIterator
   
#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
#undef VectorRIterator

end module mapl_RoutehandleVector
