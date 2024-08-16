module pFIO_AbstractSocketVectorMod
   use pFIO_AbstractSocketMod

#define _type class(AbstractSocket)
#define _allocatable
#define _vector AbstractSocketVector
#define _iterator AbstractSocketVectorIterator
#include "templates/vector.inc"

end module pFIO_AbstractSocketVectorMod
