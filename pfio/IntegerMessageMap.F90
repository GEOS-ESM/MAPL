module pFIO_IntegerMessageMapMod
   use pFIO_AbstractMessageMod

#define Key __INTEGER
#define Key_LT(a,b) (a<b)
#define T AbstractMessage
#define T_polymorphic

#define Map IntegerMessageMap
#define MapIterator IntegerMessageMapIterator
#define Pair IntegerMessagePair
#include "map/template.inc"

end module pFIO_IntegerMessageMapMod
