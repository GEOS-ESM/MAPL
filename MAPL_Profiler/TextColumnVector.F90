module MAPL_TextColumnVector
   use MAPL_TextColumn

#define _type class(TextColumn)
#define _allocatable
#define _vector TextColumnVector
#define _iterator TextColumnVectorIterator
#include "templates/vector.inc"
   
end module MAPL_TextColumnVector
