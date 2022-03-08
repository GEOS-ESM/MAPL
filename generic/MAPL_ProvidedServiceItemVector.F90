#include "MAPL_Generic.h"

module MAPL_ProvidedServiceItemVector
  use mapl_ServiceServicesTypes, only: ProvidedServiceType

#define T ProvidedServiceType 
#define T_polymorphic
#define Vector providedServiceItemVector
#define VectorIterator providedServiceItemVectorIterator
#define VectorRIterator providedServiceItemVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module MAPL_ProvidedServiceItemVector
