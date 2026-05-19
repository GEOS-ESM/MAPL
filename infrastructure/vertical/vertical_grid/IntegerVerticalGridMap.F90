module mapl3g_IntegerVerticalGridMap
   use mapl3g_VerticalGrid

#define Key __INTEGER
#define T VerticalGrid
#define T_polymorphic
#define Map IntegerVerticalGridMap
#define MapIterator IntegerVerticalGridMapIterator

#include "map/template.inc"

#undef MapIterator
#undef Map
#undef Key
#undef T_polymorphic
#undef T
   
end module mapl3g_IntegerVerticalGridMap
