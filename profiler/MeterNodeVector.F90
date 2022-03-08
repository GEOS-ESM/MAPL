module MAPL_MeterNodeVector
   use MAPL_AbstractMeterNode

#define T AbstractMeterNode
#define T_polymorphic
#define Vector MeterNodeVector
#define VectorIterator MeterNodeVectorIterator
#define VectorRIterator MeterNodeVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module MAPL_MeterNodeVector
