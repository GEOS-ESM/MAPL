module MAPL_Integer64GridFactoryMapMod
   use MAPL_AbstractGridFactoryMod
   use, intrinsic :: iso_fortran_env, only: INT64

   ! Create a map (associative array) between regridding specs and
   ! regridders.  Regridder constructors can be expensive in time and
   ! memory, so this enables the regridding system to avoid creating
   ! duplicates.
#define Key __INTEGER64
#define T AbstractGridFactory
#define T_polymorphic

#define Map Integer64GridFactoryMap
#define MapIterator Integer64GridFactoryMapIterator
#define Pair Integer64GridFactoryPair
#include "map/template.inc" 

end module MAPL_Integer64GridFactoryMapMod
