!------------------------------------------------------------------------------
! PortIdNodeIdMap: gFTL map of PortId -> NodeId. Reused by ComponentGraph
! for public import ports, public export ports, and child-port bindings
! (spec/07-component-graph.md REQ-CG-001) - three separate instances of the
! same map type, since all three are "named port identity resolves to the
! NodeId currently bound to it."
!------------------------------------------------------------------------------
module mapl_PortIdNodeIdMap_mod
   use mapl_PortId_mod, only: PortId, operator(<)
   use mapl_NodeId_mod, only: NodeId

#define Key PortId
#define Key_LT(a,b) (a < b)
#define T NodeId
#define Map PortIdNodeIdMap
#define MapIterator PortIdNodeIdMapIterator
#define Pair PortIdNodeIdPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_PortIdNodeIdMap_mod
