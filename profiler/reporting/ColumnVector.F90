module MAPL_ColumnVector
   use MAPL_AbstractColumn

#define T AbstractColumn
#define T_polymorphic
#define Vector ColumnVector
#define VectorIterator ColumnVectorIterator
#define VectorRIterator ColumnVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module MAPL_ColumnVector
