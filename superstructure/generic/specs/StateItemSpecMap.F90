module mapl_StateItemSpecMap
   use mapl_StateItemSpec

#define MAPL_DEBUG
   
#define Key __CHARACTER_DEFERRED
#define T StateItemSpec
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

end module mapl_StateItemSpecMap
