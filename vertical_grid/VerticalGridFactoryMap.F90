module mapl3g_VerticalGridFactoryMap
   use mapl3g_VerticalGridFactory

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
   
end module mapl3g_VerticalGridFactoryMap
