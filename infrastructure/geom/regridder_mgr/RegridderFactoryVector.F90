module mapl_RegridderFactoryVector_mod
   use mapl_RegridderFactory_mod

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

end module mapl_RegridderFactoryVector_mod
