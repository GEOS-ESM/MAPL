module pFIO_StringClientThreadMapMod
   use pFIO_ClientThreadMod

   ! Create a map (associative array) between names and ClientThread objects.

#define Key __CHARACTER_DEFERRED
#define T ClientThread
#define T_polymorphic
#define Map StringClientThreadMap
#define MapIterator StringClientThreadMapIterator
#define MapPair StringClientThreadMapPair

#include "map/template.inc"

#undef MapPair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module pFIO_StringClientThreadMapMod
