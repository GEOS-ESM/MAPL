module mapl_VarConnVector
   use mapl_VarConnType, only: VarConnType
   
#define T VarConnType
#define Vector VarConnVector
#define VectorIterator VarConnVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T
end module mapl_VarConnVector
