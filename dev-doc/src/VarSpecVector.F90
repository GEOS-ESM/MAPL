module mapl_VarSpecVector
   use mapl_VarSpecMod, only: MAPL_VarSpec

#define T MAPL_VarSpec
#define Vector VarSpecVector
#define VectorIterator VarSpecVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
   
end module mapl_VarSpecVector
