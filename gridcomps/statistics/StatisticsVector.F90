module mapl_StatisticsVector_mod
   use mapl_AbstractTimeStatistic_mod

#define T AbstractTimeStatistic
#define T_polymorphic
#define Vector StatisticsVector
#define VectorIterator StatisticsVectorIterator

#include "vector/template.inc"


#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T
   
end module mapl_StatisticsVector_mod
