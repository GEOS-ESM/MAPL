module mapl_MeterNodeVector_mod
   use mapl_AbstractMeterNode_mod

#define T AbstractMeterNode
#define T_polymorphic
#define Vector MeterNodeVector
#define VectorIterator MeterNodeVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T
   
end module mapl_MeterNodeVector_mod
