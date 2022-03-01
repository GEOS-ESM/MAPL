module mapl_RegridderSpecRouteHandleMap
   use mapl_RegridderSpec
   use ESMF


#define Key RegridderSpec
#define Key_LT(a,b) (a<b)
#define T ESMF_RouteHandle

#define Map RegridderSpecRouteHandleMap
#define MapIterator RegridderSpecRouteHandleMapIterator
#define Pair RegridderSpecRouteHandlePair
#include "map/template.inc"

end module mapl_RegridderSpecRouteHandleMap

