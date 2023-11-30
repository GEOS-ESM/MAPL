module mapl3g_IntegerMaplGeomMap
   use mapl3g_MaplGeom

#define Key __INTEGER
#define T MaplGeom
#define Map IntegerMaplGeomMap
#define Pair IntegerMaplGeomPair
#define MapIterator IntegerMaplGeomMapIterator

#include "map/template.inc"

#undef MapIterator
#undef Pair
#undef Map
#undef Key
#undef T
   
end module mapl3g_IntegerMaplGeomMap
