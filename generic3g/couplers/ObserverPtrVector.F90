module mapl3g_ObserverPtrVector
   use mapl3g_Observer

#define T ObserverPtr
#define Vector ObserverPtrVector
#define VectorIterator ObserverPtrVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_ObserverPtrVector
