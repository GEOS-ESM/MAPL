module MAPL_MeterNodeStack
   use MAPL_AbstractMeterNode

#define _type  class (AbstractMeterNode)
#define _pointer
#define _vector MeterNodeStack
#define _iterator MeterNodeStackIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _pointer
#undef _type
   
end module MAPL_MeterNodeStack
