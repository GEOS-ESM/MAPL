!------------------------------------------------------------------------------
! CallbackArgumentSpecMap: gFTL map of argument name (string) ->
! CallbackArgumentSpec, used by CallbackInterface for its declared
! callback arguments (spec/15-callbacks.md REQ-CB-003). Mirrors
! containers/ArgumentSpecMap.F90 exactly - kept in its own module for the
! same reason: the gFTL map/template.inc emits its own implicit
! none/private directives, which conflict with a preceding one in the
! including module.
!------------------------------------------------------------------------------
module mapl_CallbackArgumentSpecMap_mod
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec

#define Key __CHARACTER_DEFERRED
#define T CallbackArgumentSpec
#define Map CallbackArgumentSpecMap
#define MapIterator CallbackArgumentSpecMapIterator
#define Pair CallbackArgumentSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_CallbackArgumentSpecMap_mod
