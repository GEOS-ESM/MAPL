module mapl3g_ComponentDriverVector
   use mapl3g_ComponentDriver

#define T ComponentDriver
#define T_polymorphic
#define Vector ComponentDriverVector
#define VectorIterator ComponentDriverVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module mapl3g_ComponentDriverVector
