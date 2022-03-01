module MAPL_RegridderTypeSpecRegridderMapMod
   use MAPL_RegridderTypeSpec, only: RegridderTypeSpec
   use MAPL_AbstractRegridderMod

#define Key RegridderTypeSpec
#define Key_LT(a,b) (a<b)
#define T AbstractRegridder
#define T_polymorphic
#define Map RegridderTypeSpecRegridderMap
#define MapIterator RegridderTypeSpecRegridderMapIterator
#define Pair RegridderTypeSpecRegridderPair
#include "map/template.inc"

end module MAPL_RegridderTypeSpecRegridderMapMod
