module mapl3g_ConnectionVector
   use mapl3g_StateRegistry, only: Connection

#define T Connection
#define T_polymorphic
#define Vector ConnectionVector
#define VectorIterator ConnectionVectorIterator

#include "vector/template.inc"

#undef T_polymorphic
#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_ConnectionVector
