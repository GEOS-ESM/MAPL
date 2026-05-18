module MAPL_ColumnVector
   use MAPL_AbstractColumn

#define _type class(AbstractColumn)
#define _allocatable
#define _vector ColumnVector
#define _iterator ColumnVectorIterator
#include "templates/vector.inc"
   
end module MAPL_ColumnVector
