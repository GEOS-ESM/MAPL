module mapl_VariableSpecVector
   use mapl_VariableSpec

#define T VariableSpec
#define Vector VariableSpecVector
#define VectorIterator VariableSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_VariableSpecVector
