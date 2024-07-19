module mapl3g_StateItemExtensionVector
   use mapl3g_StateItemExtension
  
#define T StateItemExtension
#define T_deferred
#define Vector StateItemExtensionVector
#define VectorIterator StateItemExtensionVectorIterator

#include "vector/template.inc"

#undef T
#undef T_allocatable
#undef Vector
#undef VectorIterator
  
end module mapl3g_StateItemExtensionVector
