module mapl3g_ObservablePtrVector
   use mapl3g_Observable

#define T ObservablePtr
#define Vector ObservablePtrVector
#define VectorIterator ObservablePtrVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_ObservablePtrVector
