module mapl3g_ComponentHandlerVector
   use mapl3g_AbstractComponentHandler

#define T AbstractComponentHandler
#define T_polymorphic
#define Vector ComponentHandlerVector
#define VectorIterator ComponentHandlerVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module mapl3g_ComponentHandlerVector
