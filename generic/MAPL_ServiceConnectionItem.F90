#include "MAPL_Generic.h"

module MAPL_ServiceConnectionItemVectorMod
  use MAPL_VariableSpecification, only: ServiceConnectionType

#define _type type(ServiceConnectionType)
#define _allocatable
#define _vector serviceConnectionItemVector
#define _iterator serviceConnectionItemVectorIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _type_type

end module MAPL_ServiceConnectionItemVectorMod
