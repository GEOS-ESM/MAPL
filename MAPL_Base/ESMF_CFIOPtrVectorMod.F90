module ESMF_CFIOPtrVectorMod
  use ESMF_CFIOMod

#define _type type(ESMF_CFIO)
#define _pointer

#define _vector ESMF_CFIOPtrVector
#define _iterator ESMF_CFIOPtrVectorIterator
#include "templates/vector.inc"

end module ESMF_CFIOPtrVectorMod
