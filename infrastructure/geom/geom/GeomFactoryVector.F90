module mapl_GeomFactoryVector
   use mapl_GeomFactory

#define T GeomFactory
#define T_polymorphic
#define Vector GeomFactoryVector
#define VectorIterator GeomFactoryVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module mapl_GeomFactoryVector
