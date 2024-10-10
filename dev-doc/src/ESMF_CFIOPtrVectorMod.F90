module ESMF_CFIOPtrVectorMod
  use ESMF_CFIOMod

#define _type type(ESMF_CFIO)
#define _pointer

#define _vector ESMF_CFIOPtrVector
#define _iterator ESMF_CFIOPtrVectorIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _pointer
#undef _type
end module ESMF_CFIOPtrVectorMod

module PFIO_VectorMod
   use PFIO

#define _type type(FileMetadata)

#define _vector PFIO_Vector
#define _iterator PFIO_VectorIterator
#include "templates/vector.inc"

end module PFIO_VectorMod
