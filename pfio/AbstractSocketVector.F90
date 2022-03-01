module pFIO_AbstractSocketVectorMod
   use pFIO_AbstractSocketMod

#define T AbstractSocket
#define T_polymorphic
#define Vector AbstractSocketVector
#define VectorIterator AbstractSocketVectorIterator
#define VectorRIterator AbstractSocketVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module pFIO_AbstractSocketVectorMod
