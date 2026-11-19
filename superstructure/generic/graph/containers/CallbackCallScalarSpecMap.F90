!------------------------------------------------------------------------------
! CallbackCallScalarSpecMap: gFTL map of call-scalar name (string) ->
! CallbackCallScalarSpec, used by CallbackInterface for its declared call
! scalars. Same shape as containers/CallbackArgumentSpecMap.F90, and kept
! in its own module for the same reason: the gFTL map/template.inc emits
! its own implicit none/private directives, which conflict with a
! preceding one in the including module.
!------------------------------------------------------------------------------
module mapl_CallbackCallScalarSpecMap_mod
   use mapl_CallbackCallScalarSpec_mod, only: CallbackCallScalarSpec

#define Key __CHARACTER_DEFERRED
#define T CallbackCallScalarSpec
#define Map CallbackCallScalarSpecMap
#define MapIterator CallbackCallScalarSpecMapIterator
#define Pair CallbackCallScalarSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_CallbackCallScalarSpecMap_mod
