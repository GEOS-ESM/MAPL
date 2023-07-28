module mapl_CallbackMap

   use mapl_ESMF_Interfaces

#define Key __CHARACTER_DEFERRED
#define T CallbackMethodWrapper
#define Map CallbackMap
#define Pair CallbackPair
#define MapIterator CallbackMapIterator

#include "map/template.inc"

#undef MapIterator
#undef Pair
#undef Map
#undef T
#undef Key

end module mapl_CallbackMap
