module mapl3g_TransformVector
   use mapl3g_ExtensionTransform

#define T ExtensionTransform
#define T_polymorphic
#define Vector TransformVector
#define VectorIterator TransformVectorIterator

#include "vector/template.inc"

#undef T
#undef T_polymorphic
#undef Vector
#undef VectorIterator

end module mapl3g_TransformVector

