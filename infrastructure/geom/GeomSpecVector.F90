module mapl_GeomSpecVector_mod
   use mapl_GeomSpec_mod

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

end module mapl_GeomSpecVector_mod
