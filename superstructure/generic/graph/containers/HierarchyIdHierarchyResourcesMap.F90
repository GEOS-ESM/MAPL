!------------------------------------------------------------------------------
! HierarchyIdHierarchyResourcesMap: gFTL map of HierarchyId ->
! HierarchyResources, the private hierarchy-resources registry's own
! backing store (mapl_HierarchyRegistry_mod). Plain (non-polymorphic)
! mapped value - HierarchyResources is a single concrete container type,
! not a hierarchy.
!------------------------------------------------------------------------------
module mapl_HierarchyIdHierarchyResourcesMap_mod
   use mapl_HierarchyId_mod, only: HierarchyId, operator(<)
   use mapl_HierarchyResources_mod, only: HierarchyResources

#define Key HierarchyId
#define Key_LT(a,b) (a < b)
#define T HierarchyResources
#define Map HierarchyIdHierarchyResourcesMap
#define MapIterator HierarchyIdHierarchyResourcesMapIterator
#define Pair HierarchyIdHierarchyResourcesPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T
#undef Key_LT
#undef Key

end module mapl_HierarchyIdHierarchyResourcesMap_mod
