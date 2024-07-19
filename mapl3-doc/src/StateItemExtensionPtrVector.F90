module mapl3g_StateItemExtensionPtrVector
   use mapl3g_StateItemExtension
  
#define T StateItemExtensionPtr
#define Vector StateItemExtensionPtrVector
#define VectorIterator StateItemExtensionPtrVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator
  
end module mapl3g_StateItemExtensionPtrVector
