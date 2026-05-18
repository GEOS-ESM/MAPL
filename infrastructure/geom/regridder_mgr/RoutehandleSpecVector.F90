module mapl3g_RoutehandleSpecVector
   use mapl3g_RoutehandleSpec

#define T RoutehandleSpec
#define T_EQ(a,b) a==b 
#define Vector RoutehandleSpecVector
#define VectorIterator RoutehandleSpecVectorIterator
#define VectorRIterator RoutehandleSpecVectorRIterator
   
#include "vector/template.inc"

#undef T
#undef T_EQ
#undef Vector
#undef VectorIterator
#undef VectorRIterator

end module mapl3g_RoutehandleSpecVector
