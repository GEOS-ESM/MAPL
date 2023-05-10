module mapl_IntegerMaplGeomMap
   use mapl_MaplGeom

#define Key __INTEGER
#define T MaplGeom
#define Map IntegerMaplGeomMap
#define MapIterator IntegerMaplGeomMapIterator

#include "map/template.inc"

#undef MapIterator
#undef Map
#undef Key
#undef T
   
end module mapl_IntegerMaplGeomMap
