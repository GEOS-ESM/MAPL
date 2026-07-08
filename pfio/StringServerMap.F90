module pFIO_StringServerMapMod
   use pFIO_BaseServerMod

   ! Create a map (associative array) between string names and BaseServer objects.
   ! Supports polymorphic server values (MpiServer, MultiGroupServer, etc.)

#define Key __CHARACTER_DEFERRED
#define T BaseServer
#define T_polymorphic
#define Map StringServerMap
#define MapIterator StringServerMapIterator
#define MapPair StringServerMapPair

#include "map/template.inc"

#undef MapPair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module pFIO_StringServerMapMod
