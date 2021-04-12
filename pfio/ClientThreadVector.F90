module pFIO_ClientThreadVectorMod
   use pFIO_ClientThreadMod

#define _type class (ClientThread)
#define _pointer
#define _vector ClientThreadVector
#define _iterator ClientThreadVectorIterator
#include "templates/vector.inc"

end module pFIO_ClientThreadVectorMod
