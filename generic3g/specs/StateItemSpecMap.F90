module mapl3g_StateItemSpecMap
   use mapl3g_AbstractStateItemSpec

#define MAPL_DEBUG
   
#define Key __CHARACTER_DEFERRED
#define T AbstractStateItemSPec
#define T_polymorphic

#define Map StateItemSpecMap
#define MapIterator StateItemSpecMapIterator
#define Pair StateItemSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl3g_StateItemSpecMap
