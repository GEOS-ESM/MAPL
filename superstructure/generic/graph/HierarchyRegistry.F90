#include "MAPL.h"

!------------------------------------------------------------------------------
! HierarchyRegistry: the one shared, cross-ComponentGraph lookup point for
! HierarchyResources values (openspec/changes/hierarchy-wide-component-
! graph, design.md D1). Mirrors mapl_CallbackInterfaceRegistry_mod's own
! wrapper-procedure convention (REQ-CB-010's "narrower wrapper procedures
! that hide the singleton"), but keyed by HierarchyId rather than being a
! bare singleton - this is what lets multiple independent hierarchies
! (e.g. separate pFUnit test fixtures constructed in the same process,
! which pFUnit already does routinely) each get their own distinct
! HierarchyResources entry, with no accidental cross-hierarchy state
! bleed, while still avoiding any pointer threaded through
! OuterMetaComponent's own constructor/init_meta argument lists.
!
! A private module-level HierarchyIdHierarchyResourcesMap holds every
! live hierarchy's own resources. Three public entry points:
!   - request_new_hierarchy(): mints a fresh HierarchyId, allocates a
!     fresh (default-initialized) HierarchyResources entry, returns the
!     id. Called once by a hierarchy's root OuterMetaComponent at
!     construction, and once by ComponentGraph's own no-argument
!     constructor (today's ~40 existing standalone-synthetic-graph test
!     call shape, `graph = ComponentGraph()`), making that standalone
!     graph implicitly the root of a one-component "hierarchy" of its
!     own.
!   - get_hierarchy_resources(id): looked up fresh on every call - never
!     cached by any caller across statements - returning a pointer valid
!     until the next structural change to the registry's own backing
!     map (ordinary gFTL map pointer-validity contract, same as every
!     other `%at()`-returning accessor in this codebase).
!   - release_hierarchy(id): erases that one entry. Safe to call while
!     other hierarchies' entries remain live in the same registry - the
!     backing gFTL map is deep-copy-on-move, so erasing one entry does
!     not invalidate a pointer held into a different entry returned by
!     an earlier get_hierarchy_resources call; only a pointer into the
!     released entry itself becomes stale, exactly as erase()'s ordinary
!     contract already implies.
!------------------------------------------------------------------------------
module mapl_HierarchyRegistry_mod
   use mapl_HierarchyId_mod, only: HierarchyId, HierarchyIdGenerator
   use mapl_HierarchyResources_mod, only: HierarchyResources
   use mapl_HierarchyIdHierarchyResourcesMap_mod, only: HierarchyIdHierarchyResourcesMap
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: request_new_hierarchy
   public :: get_hierarchy_resources
   public :: release_hierarchy

   type :: HierarchyRegistry
      private
      type(HierarchyIdHierarchyResourcesMap) :: hierarchies
      type(HierarchyIdGenerator) :: id_generator
   end type HierarchyRegistry

   ! The one shared instance. Not exposed outside this module - every
   ! external interaction goes through the wrapper procedures below.
   type(HierarchyRegistry), save, target :: the_registry

contains

   function get_registry() result(registry)
      type(HierarchyRegistry), pointer :: registry

      registry => the_registry
   end function get_registry

   ! Mints a fresh HierarchyId and inserts a default-initialized
   ! HierarchyResources entry for it, atomically.
   function request_new_hierarchy(rc) result(id)
      integer, optional, intent(out) :: rc
      type(HierarchyId) :: id

      type(HierarchyRegistry), pointer :: registry
      type(HierarchyResources) :: resources
      integer :: status

      registry => get_registry()

      id = registry%id_generator%next(status)
      _ASSERT(status == 0, 'HierarchyRegistry: HierarchyId generator exhausted')

      call registry%hierarchies%insert(id, resources)

      _RETURN(_SUCCESS)
   end function request_new_hierarchy

   function get_hierarchy_resources(id) result(resources)
      type(HierarchyId), intent(in) :: id
      type(HierarchyResources), pointer :: resources

      type(HierarchyRegistry), pointer :: registry

      registry => get_registry()
      resources => registry%hierarchies%at(id)
   end function get_hierarchy_resources

   subroutine release_hierarchy(id, rc)
      type(HierarchyId), intent(in) :: id
      integer, optional, intent(out) :: rc

      type(HierarchyRegistry), pointer :: registry
      integer :: n_erased

      registry => get_registry()
      n_erased = int(registry%hierarchies%erase(id))

      _RETURN(_SUCCESS)
   end subroutine release_hierarchy

end module mapl_HierarchyRegistry_mod
