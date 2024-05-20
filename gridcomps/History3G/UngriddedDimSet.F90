module mapl3g_ungridded_dim_set
   use mapl3g_UngriddedDim

#define T UngriddedDim
#define T_LT(A, B) less_than(A, B)
#define Set UngriddedDimSet
#define SetIterator UngriddedDimSetIterator

#include "set/template.inc"

   logical function less_than(a, b)
      type(T), intent(in) :: a, b

      less_than = (a%name < b%name)

   end function less_than

#undef T
#undef T_LT
#undef Set
#undef SetIterator

end module mapl3g_ungridded_dim_set
