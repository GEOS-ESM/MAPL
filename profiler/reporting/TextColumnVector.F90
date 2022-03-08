module MAPL_TextColumnVector
   use MAPL_TextColumn

#define T TextColumn
#define T_polymorphic
#define Vector TextColumnVector
#define VectorIterator TextColumnVectorIterator
#define VectorRIterator TextColumnVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module MAPL_TextColumnVector
