module MAPL_ESMFTimeVectorMod
   use ESMF

#define _type type(ESMF_TIME)
#define _vector ESMFTimeVector
#define _iterator ESMFTimeVectorIterator

#include "templates/vector.inc"

end module MAPL_ESMFTimeVectorMod
