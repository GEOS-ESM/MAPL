! This module uses gFTL to construct a <string, callback_wrapper> map.
module mapl_CallbackMap

   use mapl_ESMF_Interfaces

   ! gftl ...
#define Key __CHARACTER_DEFERRED
#define T CallbackMethodWrapper
#include "map/template.inc"

end module mapl_CallbackMap
