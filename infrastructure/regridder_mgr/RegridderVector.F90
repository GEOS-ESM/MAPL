module mapl_RegridderVector_mod
   use mapl_Regridder_mod

#define T Regridder
#define T_polymorphic
#define Vector RegridderVector
#define VectorIterator RegridderVectorIterator
#define VectorRIterator RegridderVectorRIterator
   
#include "vector/template.inc"

#undef T
#undef T_polymorphic
#undef Vector
#undef VectorIterator
#undef VectorRIterator

end module mapl_RegridderVector_mod
