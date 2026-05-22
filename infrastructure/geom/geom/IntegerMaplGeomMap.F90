module mapl_IntegerMaplGeomMap_mod
   use mapl_MaplGeom_mod

#define Key __INTEGER
#define T MaplGeom
#define Map IntegerMaplGeomMap
#define MapIterator IntegerMaplGeomMapIterator

#include "map/template.inc"

#undef MapIterator
#undef Map
#undef Key
#undef T
   
end module mapl_IntegerMaplGeomMap_mod
