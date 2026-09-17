!------------------------------------------------------------------------------
! StateItemMemberMap: gFTL map (string -> NodeId), used by GraphStateItem for
! its field_bundle_members/state_members components
! (spec/04-graph-value-hierarchy.md REQ-SI-006). Kept in its own module
! because the gFTL map/template.inc emits its own implicit
! none/private directives, which conflict with a preceding one in the
! including module (matches the pattern already used by
! NodeIdMockPayloadMap_mod / MAPL's StringVariableMap.F90).
!------------------------------------------------------------------------------
module mapl_StateItemMemberMap_mod
   use mapl_NodeId_mod, only: NodeId

#define Key __CHARACTER_DEFERRED
#define T NodeId
#define Map StateItemMemberMap
#define MapIterator StateItemMemberMapIterator
#define Pair StateItemMemberPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_StateItemMemberMap_mod
