module mapl_RegridderSpecVector
   use mapl_RegridderSpec

#define T RegridderSpec
#define T_EQ(a,b) a==b 
#define T_polymorphic
#define Vector RegridderSpecVector
#define VectorIterator RegridderSpecVectorIterator
#define VectorRIterator RegridderSpecVectorRIterator
   
#include "vector/template.inc"

#undef T
#undef T_EQ
#undef T_polymorphic
#undef Vector
#undef VectorIterator
#undef VectorRIterator

end module mapl_RegridderSpecVector
