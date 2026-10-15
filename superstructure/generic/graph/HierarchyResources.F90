#include "MAPL.h"

!------------------------------------------------------------------------------
! HierarchyResources: container of hierarchy-wide shared resources, held one
! per hierarchy in the private HierarchyId-keyed registry
! (mapl_HierarchyRegistry_mod). Holds one HierarchyGraph field
! (openspec/changes/hierarchy-wide-component-graph, tasks.md Group 1) and
! one component_names field (tasks.md Group 2, "Component names are unique
! across the entire hierarchy" - graph/component-hierarchy delta spec) -
! deliberately named and shaped to also hold future hierarchy-wide shared
! resources (e.g. a geometry registry) without requiring another parallel
! threading mechanism when those land; that extension is explicitly not
! done in this change (see docs/graph/spec/20-implementation-roadmap.md's
! Phase 15 entry).
!------------------------------------------------------------------------------
module mapl_HierarchyResources_mod
   use mapl_HierarchyGraph_mod, only: HierarchyGraph
   use gFTL2_StringSet, only: StringSet
   implicit none(type, external)
   private

   public :: HierarchyResources

   type :: HierarchyResources
      type(HierarchyGraph) :: graph
      ! tasks.md Group 2: every component name added anywhere in this
      ! hierarchy, consulted (and inserted into) at child-add time so a
      ! name collision anywhere in the hierarchy - not only among a new
      ! child's own siblings - is caught (graph/component-hierarchy
      ! delta spec, "Component names are unique across the entire
      ! hierarchy").
      type(StringSet) :: component_names
   end type HierarchyResources

end module mapl_HierarchyResources_mod
