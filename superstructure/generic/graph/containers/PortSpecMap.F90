!------------------------------------------------------------------------------
! PortSpecMap: gFTL map of port name (string) -> PortSpec, used by
! TransformGraphNode for its declared input/output ports
! (spec/10-transforms-and-ports.md REQ-XFORM-002/003/004). Kept in its
! own module for the same reason as mapl_StateItemMemberMap_mod: the gFTL
! map/template.inc emits its own implicit none/private directives,
! which conflict with a preceding one in the including module.
!------------------------------------------------------------------------------
module mapl_PortSpecMap_mod
   use mapl_PortSpec_mod, only: PortSpec

#define Key __CHARACTER_DEFERRED
#define T PortSpec
#define Map PortSpecMap
#define MapIterator PortSpecMapIterator
#define Pair PortSpecPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_PortSpecMap_mod
