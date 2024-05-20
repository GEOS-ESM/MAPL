module mapl3g_string_ungridded_dim_map
   use mapl3g_UngriddedDim

#include "types/key_deferredLengthString.inc"   
#define _value type(UngriddedDim)

#define _map StringUngriddedDimMap
#define _iterator StringUngriddedDimMapIterator
#define _alt
#include "templates/map.inc"

#undef _alt
#undef _iterator
#undef _map
#undef _value

end module mapl3g_string_ungridded_dim_map
