module mapl_GeomSpecVector
   use mapl_GeomSpec

#define T GeomSpec
#define T_EQ(a,b) a==b 
#define T_polymorphic
#define Vector GeomSpecVector
#define VectorIterator GeomSpecVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module mapl_GeomSpecVector
