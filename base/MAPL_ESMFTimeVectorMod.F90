module MAPL_ESMFTimeVectorMod
   use ESMF

#define T ESMF_TIME 
#define Vector ESMFTimeVector
#define VectorIterator ESMFTimeVectorIterator
#define VectorRIterator ESMFTimeVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T

end module MAPL_ESMFTimeVectorMod
