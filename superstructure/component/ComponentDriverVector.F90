module mapl_ComponentDriverVector_mod
   use mapl_ComponentDriver_mod

#define T ComponentDriver
#define T_polymorphic
#define Vector ComponentDriverVector
#define VectorIterator ComponentDriverVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module mapl_ComponentDriverVector_mod
