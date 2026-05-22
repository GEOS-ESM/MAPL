module mapl_VerticalGridFactoryMap_mod
   use mapl_VerticalGridFactory_mod

#define Key __CHARACTER_DEFERRED
#define T VerticalGridFactory
#define  T_polymorphic
#define Map VerticalGridFactoryMap
#define MapIterator VerticalGridFactoryIterator

#include "map/template.inc"

#undef MapIterator
#undef Map
#undef Key
#undef T_polymorphic
#undef T
   
end module mapl_VerticalGridFactoryMap_mod
