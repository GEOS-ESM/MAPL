module mapl3g_output_info_set
   use mapl3g_output_info

#define T OutputInfo
#define T_LT(A, B) (A) < (B)
#define Set OutputInfoSet
#define SetIterator OutputInfoSetIterator

#include "set/template.inc"

#undef T
#undef T_LT
#undef Set
#undef SetIterator

end module mapl3g_output_info_set
