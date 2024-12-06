module MAPL_RegridderTypeSpecRegridderMapMod
   use MAPL_RegridderTypeSpec, only: RegridderTypeSpec
   use MAPL_AbstractRegridderMod

#define _key type(RegridderTypeSpec)
#define _key_less_than_defined   
#define _value class (AbstractRegridder)
#define _value_allocatable

#define _map RegridderTypeSpecRegridderMap
#define _iterator RegridderTypeSpecRegridderMapIterator
#define _alt
#include "templates/map.inc"
   
end module MAPL_RegridderTypeSpecRegridderMapMod
