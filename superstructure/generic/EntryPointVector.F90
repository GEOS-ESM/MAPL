module mapl_EntryPointVector_mod

   use mapl_RunEntryPoint_mod

#define T runEntryPoint
#define Vector entryPointVector
#define VectorIterator entryPointVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_EntryPointVector_mod
