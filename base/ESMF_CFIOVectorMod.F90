module ESMF_CFIOVectorMod
  use ESMF_CFIOMod

#define T ESMF_CFIO
#define T_polymorphic
#define Vector ESMF_CFIOVector
#define VectorIterator ESMF_CFIOVectorIterator
#define VectorRIterator ESMF_CFIOVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module ESMF_CFIOVectorMod

