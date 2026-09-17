!------------------------------------------------------------------------------
! PortBindingKeyPortMap: gFTL map of PortBindingKey ->
! StateItemMemberMap (itself port name -> NodeId), the external
! Transform port-binding table storage (spec/10-transforms-and-ports.md
! REQ-XFORM-005). Reuses StateItemMemberMap's existing
! character -> NodeId container for the inner map rather than
! duplicating an identical container under a new name (the same reuse
! ComponentGraph already applies to its own resource_index).
!------------------------------------------------------------------------------
module mapl_PortBindingKeyPortMap_mod
   use mapl_PortBindingKey_mod, only: PortBindingKey, operator(<)
   use mapl_StateItemMemberMap_mod, only: StateItemMemberMap

#define Key PortBindingKey
#define Key_LT(a,b) (a < b)
#define T StateItemMemberMap
#define Map PortBindingKeyPortMap
#define MapIterator PortBindingKeyPortMapIterator
#define Pair PortBindingKeyPortPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_PortBindingKeyPortMap_mod
