module mapl3g_StatisticsVector
   use mapl3g_AbstractTimeStatistic

#define T AbstractTimeStatistic
#define T_polymorphic
#define Vector StatisticsVector
#define VectorIterator StatisticsVectorIterator

#include "vector/template.inc"


#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T
   
end module mapl3g_StatisticsVector
