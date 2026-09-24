!------------------------------------------------------------------------------
! CallbackServiceNameIdMap: gFTL map of service name (string) ->
! CallbackInterfaceId, the CallbackInterfaceRegistry's own lookup-by-name
! index (spec/15-callbacks.md REQ-CB-009).
!------------------------------------------------------------------------------
module mapl_CallbackServiceNameIdMap_mod
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId

#define Key __CHARACTER_DEFERRED
#define T CallbackInterfaceId
#define Map CallbackServiceNameIdMap
#define MapIterator CallbackServiceNameIdMapIterator
#define Pair CallbackServiceNameIdPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_CallbackServiceNameIdMap_mod
