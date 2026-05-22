module mapl_TextColumnVector_mod
   use mapl_TextColumn_mod

#define T TextColumn
#define T_polymorphic
#define Vector TextColumnVector
#define VectorIterator TextColumnVectorIterator
#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T
end module mapl_TextColumnVector_mod
