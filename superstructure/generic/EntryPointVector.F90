module mapl_EntryPointVector

   use mapl_RunEntryPoint

#define T runEntryPoint
#define Vector entryPointVector
#define VectorIterator entryPointVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_EntryPointVector
