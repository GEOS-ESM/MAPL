module pFIO_StringAttributeMapMod
   use pFIO_AttributeMod

#define Key __CHARACTER_DEFERRED
#define T Attribute
#define T_EQ(x,y) ((x) == (y))

#define Pair StringAttributePair
#define Map StringAttributeMap
#define MapIterator StringAttributeMapIterator
   
#include "map/template.inc"

#undef MapIterator
#undef Map
#undef Pair
#undef T_EQ
#undef T
#undef Key
   
end module pFIO_StringAttributeMapMod
