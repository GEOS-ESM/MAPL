module mapl_StringCompositeMap
   use mapl_AbstractComposite


#define Key __CHARACTER_DEFERRED
#define T AbstractComposite
#define T_polymorphic

#define Map StringCompositeMap
#define MapIterator StringCompositeMapIterator
#define Pair StringVariablePair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module mapl_StringCompositeMap
