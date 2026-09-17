!------------------------------------------------------------------------------
! PortNameRevisionMap: gFTL map of port name (string) -> NodeRevision.
! Used by TransformGraphNode to record the input-revision baseline from
! its last successful execution, and by the demand-driven update engine
! to assemble the current input-revision snapshot to compare against it
! (spec/11-revision-and-update.md REQ-REV-006).
!------------------------------------------------------------------------------
module mapl_PortNameRevisionMap_mod
   use mapl_NodeRevision_mod, only: NodeRevision

#define Key __CHARACTER_DEFERRED
#define T NodeRevision
#define Map PortNameRevisionMap
#define MapIterator PortNameRevisionMapIterator
#define Pair PortNameRevisionPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key

end module mapl_PortNameRevisionMap_mod
