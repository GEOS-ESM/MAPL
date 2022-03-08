module pFIO_IntegerSocketMapMod
   use pFIO_AbstractSocketMod

#define Key __INTEGER
#define T AbstractSocket 
#define T_polymorphic

#define Map IntegerSocketMap
#define MapIterator IntegerSocketMapIterator
#define Pair IntegerSocketPair
#include "map/template.inc"

end module pFIO_IntegerSocketMapMod
