module MAPL_StringGridFactoryMapMod
   use MAPL_AbstractGridFactoryMod

   ! Create a map (associative array) between regridding specs and
   ! regridders.  Regridder constructors can be expensive in time and
   ! memory, so this enables the regridding system to avoid creating
   ! duplicates.

#define Key __CHARACTER_DEFERRED
#define T AbstractGridFactory
#define T_polymorphic

#define Map StringGridFactoryMap
#define MapIterator StringGridFactoryMapIterator
#define Pair StringGridFactoryPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key
 
end module MAPL_StringGridFactoryMapMod
