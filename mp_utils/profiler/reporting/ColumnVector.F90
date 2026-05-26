module mapl_ColumnVector_mod
   use mapl_AbstractColumn_mod

#define _type class(AbstractColumn)
#define _allocatable
#define _vector ColumnVector
#define _iterator ColumnVectorIterator
#include "templates/vector.inc"
   
end module mapl_ColumnVector_mod
