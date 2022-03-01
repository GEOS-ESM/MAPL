module pFIO_ClientThreadVectorMod
   use pFIO_ClientThreadMod

#define T ClientThread
#define Vector ClientThreadVector
#define VectorIterator ClientThreadVectorIterator
#define VectorRIterator ClientThreadVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T

end module pFIO_ClientThreadVectorMod
