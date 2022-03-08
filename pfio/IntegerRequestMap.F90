module pFIO_IntegerRequestMapMod
   use pFIO_AbstractRequestHandleMod

#define Key __INTEGER
#define T AbstractRequestHandle 
#define T_polymorphic

#define Map IntegerRequestMap
#define MapIterator IntegerRequestMapIterator
#define Pair IntegerRequestPair
#include "map/template.inc"

end module pFIO_IntegerRequestMapMod
