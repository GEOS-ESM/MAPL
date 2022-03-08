#include "MAPL_Generic.h"

module MAPL_ServiceConnectionItemVector
  use mapl_ServiceServicesTypes, only: ServiceConnectionType


#define T ServiceConnectionType 
#define T_polymorphic
#define Vector serviceConnectionItemVector
#define VectorIterator serviceConnectionItemVectorIterator
#define VectorRIterator serviceConnectionItemVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module MAPL_ServiceConnectionItemVector
