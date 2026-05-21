module mapl_ConnectionVector
   use mapl_Connection

#define T Connection
#define T_polymorphic
#define Vector ConnectionVector
#define VectorIterator ConnectionVectorIterator

#include "vector/template.inc"

#undef T_polymorphic
#undef T
#undef Vector
#undef VectorIterator
  
end module mapl_ConnectionVector
