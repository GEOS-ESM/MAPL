!------------------------------------------------------------------------------
! PortIdStringMap: gFTL map of PortId -> character(:) (deferred-length
! string). Used by ComponentGraph as a sparse side-table recording an
! export port's own qualified name override, when one was assigned by
! export propagation (graphbuilder_run_export_propagate_hook) rather than
! by direct advertisement - see ComponentGraph.F90's own
! graph_get_export_port_name header comment for the full rationale
! (openspec/changes/hierarchy-wide-component-graph, tasks.md 6.8).
!------------------------------------------------------------------------------
module mapl_PortIdStringMap_mod
   use mapl_PortId_mod, only: PortId, operator(<)

#define Key PortId
#define Key_LT(a,b) (a < b)
#define T __CHARACTER_DEFERRED
#define Map PortIdStringMap
#define MapIterator PortIdStringMapIterator
#define Pair PortIdStringPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_PortIdStringMap_mod
