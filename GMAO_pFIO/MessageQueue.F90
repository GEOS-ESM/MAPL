module pFIO_MessageQueueMod
   use pFIO_AbstractMessageMod

#define _type class(AbstractMessage)
#define _allocatable
#define _vector MessageQueue
#define _iterator MessageQueueIterator
#define _niterator MessageQueueRIterator
#include "templates/vector.inc"

end module pFIO_MessageQueueMod
