module mapl3g_ungridded_dim_set
   use mapl3g_ungridded_dim_info

#define T UngriddedDimInfo
#define T_LT(A, B) (A) < (B)
#define Set UngriddedDimInfoSet
#define SetIterator UngriddedDimInfoSetIterator

#include "set/template.inc"

#undef T
#undef T_LT
#undef Set
#undef SetIterator

end module mapl3g_ungridded_dim_set
