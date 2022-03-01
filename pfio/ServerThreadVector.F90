module pFIO_ServerThreadVectorMod
   use pFIO_ServerThreadMod

#define T ServerThread
#define Vector ServerThreadVector
#define VectorIterator ServerThreadVectorIterator
#define VectorRIterator ServerThreadVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T

end module pFIO_ServerThreadVectorMod
