!------------------------------------------------------------------------------
! CallbackArgumentOptionsMap: gFTL map of argument name (string) ->
! CallbackArgumentOptions, used by CallbackMethodSpec beside its
! AccessSpecMap. Kept in its own module because the gFTL map/template.inc
! emits its own implicit none/private directives, which conflict with a
! preceding one in the including module.
!------------------------------------------------------------------------------
module mapl_CallbackArgumentOptionsMap_mod
   use mapl_CallbackArgumentOptions_mod, only: CallbackArgumentOptions

#define Key __CHARACTER_DEFERRED
#define T CallbackArgumentOptions
#define Map CallbackArgumentOptionsMap
#define MapIterator CallbackArgumentOptionsMapIterator
#define Pair CallbackArgumentOptionsPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_CallbackArgumentOptionsMap_mod
