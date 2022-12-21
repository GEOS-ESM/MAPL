! This module uses gFTL to construct a <string, callback_wrapper> map.
module mapl_CallbackMap

   use mapl_ESMF_Interfaces

   ! gftl ...
#define Key __CHARACTER_DEFERRED
#define T CallbackMethodWrapper
#define Map CallbackMap
#define MapIterator CallbackMapIterator

#include "map/template.inc"

#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_CallbackMap
