module mapl_StringCompositeMap
   use mapl_AbstractComposite
#include "types/key_deferredLengthString.inc"
#define _value class(AbstractComposite)
#define _value_allocatable class(AbstractComposite)
#define _map StringCompositeMap
#define _iterator StringCompositeMapIterator
#define _pair StringCompositePair
#define _alt

#include "templates/map.inc"

end module mapl_StringCompositeMap
