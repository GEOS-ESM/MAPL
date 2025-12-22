module MAPL_MeterNodeVector
   use MAPL_AbstractMeterNode

#define T AbstractMeterNode
#define T_polymorphic
#define Vector MeterNodeVector
#define VectorIterator MeterNodeVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T
   
end module MAPL_MeterNodeVector
