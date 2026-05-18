module mapl3g_EntryPointVector

   use mapl3g_RunEntryPoint

#define T runEntryPoint
#define Vector entryPointVector
#define VectorIterator entryPointVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl3g_EntryPointVector
