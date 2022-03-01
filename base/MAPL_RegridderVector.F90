module MAPL_RegridderVectorMod
   use MAPL_AbstractRegridderMod

#define T AbstractRegridder
#define T_polymorphic
#define Vector RegridderVector
#define VectorIterator RegridderVectorIterator
#define VectorRIterator RegridderVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T
  

end module MAPL_RegridderVectorMod
