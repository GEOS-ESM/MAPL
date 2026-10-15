#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) init_meta_smod
   use mapl_ErrorHandling_mod
   use mapl_HierarchyRegistry_mod, only: request_new_hierarchy, get_hierarchy_resources
   use mapl_HierarchyResources_mod, only: HierarchyResources
   use pFlogger, only: logging
   implicit none(type,external)

contains

   ! NOTE: _Not_ an ESMF phase - this is initializing the object itself.
   ! Constructor (new_outer_meta) only copies basic parameters.  All
   ! other initialization is in this procedure.

   module subroutine init_meta(this, hierarchy_id, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(HierarchyId), optional, intent(in) :: hierarchy_id
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: user_gc_name
      type(HierarchyResources), pointer :: resources

      user_gc_name = this%user_gc_driver%get_name(_RC)
      this%registry = StateRegistry(user_gc_name)

      ! openspec/changes/hierarchy-wide-component-graph, design.md D1:
      ! a root component (no hierarchy_id supplied by its caller)
      ! requests a fresh hierarchy of its own; a descendant
      ! (add_child_by_spec.F90) always supplies its parent's own
      ! hierarchy_id, so this%local_graph below resolves against the
      ! same shared HierarchyGraph as every other component in the
      ! hierarchy.
      if (present(hierarchy_id)) then
         this%hierarchy_id = hierarchy_id
      else
         this%hierarchy_id = request_new_hierarchy()
         ! graph/component-hierarchy delta spec "Component names are
         ! unique across the entire hierarchy": this component is the
         ! hierarchy's own root - no add_child_by_spec call ever runs
         ! for it, so nothing else ever records ITS OWN name into the
         ! hierarchy-wide registry. Done only here, and only in this
         ! branch (a non-root descendant's name is instead recorded by
         ! add_child_by_spec itself, once construction has fully
         ! succeeded) - never both, avoiding a double-insert for the
         ! same component. user_gc_name (not this%get_name(), which
         ! returns the bracketed OUTER wrapper gridcomp's own name,
         ! "[name]") matches the plain, unbracketed name
         ! add_child_by_spec inserts for every other component.
         resources => get_hierarchy_resources(this%hierarchy_id)
         call resources%component_names%insert(user_gc_name)
      end if

      ! docs/graph/spec/02-component-hierarchy.md REQ-HIER-002/004: every
      ! OuterComponent owns exactly one local ComponentGraph. Constructed
      ! here (not in new_outer_meta) to match the registry's construction
      ! pattern above - ComponentGraph() is non-trivial (seeds the default
      ! network id) so it must be called explicitly, not left to
      ! component-wise default initialization.
      this%local_graph = ComponentGraph(this%hierarchy_id)

      this%lgr => logging%get_logger(user_gc_name)

      _RETURN(_SUCCESS)

   end subroutine init_meta

end submodule init_meta_smod
