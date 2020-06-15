module MAPL_StringGridFactoryMapMod
   use MAPL_AbstractGridFactoryMod

   ! Create a map (associative array) between regridding specs and
   ! regridders.  Regridder constructors can be expensive in time and
   ! memory, so this enables the regridding system to avoid creating
   ! duplicates.

#include "types/key_deferredLengthString.inc"   
#define _value class(AbstractGridFactory)
#define _value_allocatable

#define _map StringGridFactoryMap
#define _iterator StringGridFactoryMapIterator
#define _alt
#include "templates/map.inc"
   
end module MAPL_StringGridFactoryMapMod
