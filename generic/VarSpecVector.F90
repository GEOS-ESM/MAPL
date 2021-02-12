module mapl_VarSpecVector
   use mapl_VariableSpecification, only: MAPL_VarSpec

#define _type type(MAPL_VarSpec)
#define _vector VarSpecVector
#define _iterator VarSpecVectorIterator
#include "templates/vector.inc"
#undef _iterator
#undef _vector
#undef _type
   
end module mapl_VarSpecVector
