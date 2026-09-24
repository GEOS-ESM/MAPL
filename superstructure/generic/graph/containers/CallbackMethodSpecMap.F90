!------------------------------------------------------------------------------
! CallbackMethodSpecMap: gFTL map of method name (string) ->
! CallbackMethodSpec, used by CallbackInterface for its declared
! callback methods (spec/15-callbacks.md REQ-CB-003). Mirrors
! containers/ArgumentSpecMap.F90 exactly - kept in its own module for the
! same reason: the gFTL map/template.inc emits its own implicit
! none/private directives, which conflict with a preceding one in the
! including module.
!------------------------------------------------------------------------------
module mapl_CallbackMethodSpecMap_mod
   use mapl_CallbackMethodSpec_mod, only: CallbackMethodSpec

#define Key __CHARACTER_DEFERRED
#define T CallbackMethodSpec
#define Map CallbackMethodSpecMap
#define MapIterator CallbackMethodSpecMapIterator
#define Pair CallbackMethodSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_CallbackMethodSpecMap_mod
