!------------------------------------------------------------------------------
! CallbackMethodAttachmentMap: gFTL map of method name (string) ->
! StateMethodInvocation, used by CallbackStateBinding for its "method
! name -> method attachment" storage (spec/15-callbacks.md REQ-CB-007).
! The map value type is Phase 4a's own existing StateMethodInvocation
! adapter, not a new wrapper type - a callback method attachment *is*
! exactly "an invocation adapter bound to this method name on this
! callback State's NodeId" (callback-data-model-registry design.md
! Decision 5).
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
