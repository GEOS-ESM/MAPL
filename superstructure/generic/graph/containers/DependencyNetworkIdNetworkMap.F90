!------------------------------------------------------------------------------
! DependencyNetworkIdNetworkMap: gFTL map of DependencyNetworkId ->
! DependencyNetwork, the ComponentGraph-owned network registry
! (spec/07-component-graph.md REQ-CG-001). Plain (non-polymorphic) mapped
! value - DependencyNetwork is a single concrete type, not a hierarchy.
!------------------------------------------------------------------------------
module mapl_DependencyNetworkIdNetworkMap_mod
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId, operator(<)
   use mapl_DependencyNetwork_mod, only: DependencyNetwork

#define Key DependencyNetworkId
#define Key_LT(a,b) (a < b)
#define T DependencyNetwork
#define Map DependencyNetworkIdNetworkMap
#define MapIterator DependencyNetworkIdNetworkMapIterator
#define Pair DependencyNetworkIdNetworkPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_DependencyNetworkIdNetworkMap_mod
