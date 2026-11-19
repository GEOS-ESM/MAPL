!------------------------------------------------------------------------------
! DependencyNetworkIdSet: gFTL set of DependencyNetworkId, used by
! ComponentGraph%get_network_ids() (spec/07-component-graph.md, added
! for the graph-neutral exporter's REQ-VIZ-003 dependency on public
! full-graph enumeration).
!------------------------------------------------------------------------------
module mapl_DependencyNetworkIdSet_mod
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId, operator(<)

#define T DependencyNetworkId
#define T_LT(a,b) (a < b)
#define Set DependencyNetworkIdSet
#define SetIterator DependencyNetworkIdSetIterator

#include "set/template.inc"

#undef SetIterator
#undef Set
#undef T_LT
#undef T

end module mapl_DependencyNetworkIdSet_mod
