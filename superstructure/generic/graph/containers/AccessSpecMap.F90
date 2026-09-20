!------------------------------------------------------------------------------
! AccessSpecMap: gFTL map of argument name (string) -> AccessSpec. A
! generic character -> AccessSpec map with no callback-specific meaning
! of its own (AccessSpec is itself a general MAPL concept, spec/15-
! callbacks.md REQ-CB-005) - introduced as a standalone container
! (callback-data-model-registry design.md Decision 2) rather than nested
! inside CallbackMethodSpec.F90, since the gFTL map/template.inc emits
! its own implicit none/private directives, which conflict with a
! preceding one in the including module (same reason ArgumentSpecMap.F90
! and every other map wrapper in this directory is kept standalone).
!------------------------------------------------------------------------------
module mapl_AccessSpecMap_mod
   use mapl_AccessSpec_mod, only: AccessSpec

#define Key __CHARACTER_DEFERRED
#define T AccessSpec
#define Map AccessSpecMap
#define MapIterator AccessSpecMapIterator
#define Pair AccessSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_AccessSpecMap_mod
