#include "MAPL_Generic.h"

module MAPL_ProvidedServiceItemVector
  use mapl_ServiceServicesTypes, only: ProvidedServiceType

#define _type type(ProvidedServiceType)
#define _allocatable
#define _vector providedServiceItemVector
#define _iterator providedServiceItemVectorIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _type_type

end module MAPL_ProvidedServiceItemVector
