module mapl3g_ExtensionVector
   use mapl3g_StateExtension

#define T StateExtension
#define Vector ExtensionVector
#define VectorIterator ExtensionVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl3g_ExtensionVector
