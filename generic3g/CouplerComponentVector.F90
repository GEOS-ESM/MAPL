module mapl3g_CouplerComponentVector
   use mapl3g_GenericCouplerComponent

#define T GenericCouplerComponent
#define Vector CouplerComponentVector
#define VectorIterator CouplerComponentVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl3g_CouplerComponentVector
