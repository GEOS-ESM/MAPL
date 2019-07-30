module MAPL_RegridderVectorMod
   use MAPL_AbstractRegridderMod

#define _type class (AbstractRegridder)
#define _allocatable
#define _vector RegridderVector
#define _iterator RegridderVectorIterator
#include "templates/vector.inc"   

end module MAPL_RegridderVectorMod
