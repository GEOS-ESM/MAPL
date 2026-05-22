module mapl_VariableSpecVector_mod
   use mapl_VariableSpec_mod

#define T VariableSpec
#define Vector VariableSpecVector
#define VectorIterator VariableSpecVectorIterator

#include "vector/template.inc"

#undef T
#undef Vector
#undef VectorIterator

end module mapl_VariableSpecVector_mod
