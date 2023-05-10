module mapl_GeomSpecIntegerMap
   use mapl_GeomSpec

#define T __INTEGER32
#define Key GeomSpec
#define Map GeomSpecIntegerMap
#define MapIterator GeomSpecIntegerMapIterator

#include "map/template.inc"

#undef MapIterator
#undef Map
#undef Key
#undef T
   
end module mapl_GeomSpecIntegerMap
