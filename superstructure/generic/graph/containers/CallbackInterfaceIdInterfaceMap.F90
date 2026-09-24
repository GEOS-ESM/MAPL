!------------------------------------------------------------------------------
! CallbackInterfaceIdInterfaceMap: gFTL map of CallbackInterfaceId ->
! CallbackInterface, the CallbackInterfaceRegistry's own identity ->
! interface storage (spec/15-callbacks.md REQ-CB-009). Mirrors
! DependencyNetworkIdNetworkMap.F90's own "Id -> owned value" shape:
! Key_LT ordering comes from CallbackInterfaceId's own IdTemplate.inc-
! generated operator(<).
!------------------------------------------------------------------------------
module mapl_CallbackInterfaceIdInterfaceMap_mod
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId, operator(<)
   use mapl_CallbackInterface_mod, only: CallbackInterface

#define Key CallbackInterfaceId
#define Key_LT(a,b) (a < b)
#define T CallbackInterface
#define Map CallbackInterfaceIdInterfaceMap
#define MapIterator CallbackInterfaceIdInterfaceMapIterator
#define Pair CallbackInterfaceIdInterfacePair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_CallbackInterfaceIdInterfaceMap_mod
