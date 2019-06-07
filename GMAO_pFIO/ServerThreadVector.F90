module pFIO_ServerThreadVectorMod
   use pFIO_ServerThreadMod

#define _type type (ServerThread)
#define _pointer
#define _vector ServerThreadVector
#define _iterator ServerThreadVectorIterator
#include "templates/vector.inc"

end module pFIO_ServerThreadVectorMod
