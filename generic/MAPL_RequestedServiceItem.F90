#include "MAPL_Generic.h"

module MAPL_RequestedServiceItemVectorMod
  use MAPL_VariableSpecification, only: RequestedServiceType

#define _type type(RequestedServiceType)
#define _allocatable
#define _vector requestedServiceItemVector
#define _iterator requestedServiceItemVectorIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _type_type

end module MAPL_RequestedServiceItemVectorMod
