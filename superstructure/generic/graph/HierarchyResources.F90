#include "MAPL.h"

!------------------------------------------------------------------------------
! HierarchyResources: container of hierarchy-wide shared resources, held one
! per hierarchy in the private HierarchyId-keyed registry
! (mapl_HierarchyRegistry_mod). Holds one HierarchyGraph and one
! component_names set, and is shaped so future hierarchy-wide resources (e.g.
! a geometry registry) can be added without another threading mechanism.
!------------------------------------------------------------------------------
module mapl_HierarchyResources_mod
   use mapl_HierarchyGraph_mod, only: HierarchyGraph
   use gFTL2_StringSet, only: StringSet
   implicit none(type, external)
   private

   public :: HierarchyResources

   type :: HierarchyResources
      type(HierarchyGraph) :: graph
      ! Every component name added anywhere in this hierarchy, consulted and
      ! inserted into at child-add time so a name collision anywhere in the
      ! hierarchy, not only among a new child's siblings, is caught.
      type(StringSet) :: component_names
   end type HierarchyResources

end module mapl_HierarchyResources_mod
