module mapl3g_RegridderFactoryVector
   use mapl3g_RegridderFactory

#define T RegridderFactory
#define T_polymorphic
#define Vector RegridderFactoryVector
#define VectorIterator RegridderFactoryVectorIterator
#define VectorRIterator RegridderFactoryVectorRIterator
   
#include "vector/template.inc"

#undef T
#undef T_polymorphic
#undef Vector
#undef VectorIterator
#undef VectorRIterator

end module mapl3g_RegridderFactoryVector
