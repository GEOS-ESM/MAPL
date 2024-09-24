module pFIO_IntegerRequestMapMod
   use pFIO_AbstractRequestHandleMod

#define Key __INTEGER
#define T AbstractRequestHandle
#define T_polymorphic
#define Pair IntegerRequestPair
#define Map IntegerRequestMap
#define MapIterator IntegerRequestMapIterator

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef T_polymorphic
#undef Key

end module pFIO_IntegerRequestMapMod
