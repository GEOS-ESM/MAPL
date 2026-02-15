module mapl3g_ExtDataRuleMap
   use mapl3g_ExtDataRule

#define Key __CHARACTER_DEFERRED
#define T ExtDataRule
#define Map ExtDataRuleMap
#define MapIterator ExtDataRuleMapIterator
#define Pair ExtDataRulePair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl3g_ExtDataRuleMap


