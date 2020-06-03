module MAPL_Integer64GridFactoryMapMod
   use MAPL_AbstractGridFactoryMod
   use, intrinsic :: iso_fortran_env, only: INT64

   ! Create a map (associative array) between regridding specs and
   ! regridders.  Regridder constructors can be expensive in time and
   ! memory, so this enables the regridding system to avoid creating
   ! duplicates.

#  define _key integer(kind=INT64)
#  define _value class(AbstractGridFactory)
#  define _value_allocatable

#  define _map Integer64GridFactoryMap
#  define _iterator Integer64GridFactoryMapIterator
#  define _alt
#  include "templates/map.inc"
   
end module MAPL_Integer64GridFactoryMapMod
