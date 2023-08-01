module MAPL_MeterNodeVector
   use MAPL_AbstractMeterNode

#define _type  class (AbstractMeterNode)
#define _allocatable
#define _vector MeterNodeVector
#define _iterator MeterNodeVectorIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _pointer
#undef _type
   
end module MAPL_MeterNodeVector
