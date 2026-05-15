module mapl3g_RegridderVector
   use mapl3g_Regridder

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

end module mapl3g_RegridderVector
