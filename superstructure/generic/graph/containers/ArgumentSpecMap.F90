!------------------------------------------------------------------------------
! ArgumentSpecMap: gFTL map of argument name (string) -> ArgumentSpec,
! used by MethodGraphNode for its declared named arguments
! (spec/12-methods-and-drivers.md REQ-MTH-002). Mirrors
! containers/PortSpecMap.F90 exactly - kept in its own module for the
! same reason: the gFTL map/template.inc emits its own implicit
! none/private directives, which conflict with a preceding one in the
! including module.
!------------------------------------------------------------------------------
module mapl_ArgumentSpecMap_mod
   use mapl_ArgumentSpec_mod, only: ArgumentSpec

#define Key __CHARACTER_DEFERRED
#define T ArgumentSpec
#define Map ArgumentSpecMap
#define MapIterator ArgumentSpecMapIterator
#define Pair ArgumentSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_ArgumentSpecMap_mod
