#include "MAPL_Generic.h"

module MAPL_RequestedServiceItemVector
  use mapl_ServiceServicesTypes, only: RequestedServiceType


#define T RequestedServiceType 
#define T_polymorphic
#define Vector requestedServiceItemVector
#define VectorIterator requestedServiceItemVectorIterator
#define VectorRIterator requestedServiceItemVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module MAPL_RequestedServiceItemVector
