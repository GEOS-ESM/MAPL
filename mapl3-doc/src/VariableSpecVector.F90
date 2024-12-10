module mapl3g_VariableSpecVector
   use mapl3g_VariableSpec

#define T VariableSpec
#define Vector VariableSpecVector
#define VectorIterator VariableSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl3g_VariableSpecVector
