module mapl3g_StatisticsMap
   use mapl3g_AbstractTimeStatistic

#define Key __CHARACTER_DEFERRED
#define T AbstractTimeStatistic
#define T_polymorphic
#define Map StatisticsMap
#define MapIterator StatisticsMapIterator
#define Pair StatisticsMapPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

   
end module mapl3g_StatisticsMap
