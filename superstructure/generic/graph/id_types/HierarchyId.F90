!------------------------------------------------------------------------------
! HierarchyId: encapsulated identity type distinguishing one component
! hierarchy from another, used to key the private hierarchy-resources
! registry (openspec/changes/hierarchy-wide-component-graph, design.md D1).
!
! Generated from the authoritative template spec/templates/IdTemplate.inc,
! exactly like NodeId/DependencyNetworkId - a leaf value type with no
! dependency on OuterMetaComponent, ComponentGraph, or any hierarchy-graph
! storage type.
!------------------------------------------------------------------------------
module mapl_HierarchyId_mod

#define ID_NAME HierarchyId
#include "IdTemplate.inc"

end module mapl_HierarchyId_mod
