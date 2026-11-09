!------------------------------------------------------------------------------
! CallbackMethodAttachmentMap: gFTL map of method name (string) ->
! StateMethodInvocation, used by CallbackStateBinding for its "method
! name -> method attachment" storage (spec/15-callbacks.md).
! The map value type is the existing StateMethodInvocation
! adapter, not a new wrapper type - a callback method attachment *is*
! exactly "an invocation adapter bound to this method name on this
! callback State's NodeId".
!------------------------------------------------------------------------------
module mapl_CallbackMethodAttachmentMap_mod
   use mapl_StateMethodInvocation_mod, only: StateMethodInvocation

#define Key __CHARACTER_DEFERRED
#define T StateMethodInvocation
#define Map CallbackMethodAttachmentMap
#define MapIterator CallbackMethodAttachmentMapIterator
#define Pair CallbackMethodAttachmentPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_CallbackMethodAttachmentMap_mod
