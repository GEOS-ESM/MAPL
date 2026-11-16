#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) add_child_by_spec_smod

   use mapl_ChildSpec_mod
   use mapl_HierarchyResources_mod, only: HierarchyResources
   use mapl_HierarchyRegistry_mod, only: get_hierarchy_resources
   use mapl_ErrorHandling_mod
   use esmf

   implicit none(type,external)

contains

   ! Thin wrapper over create_child: owns the duplicate checks, the child map,
   ! the hierarchy-wide name registry and the logging. The construction itself
   ! is shared with other parent meta types.
   module recursive subroutine add_child_by_spec(this, child_name, child_spec, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      type(ChildSpec), intent(inout) :: child_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child_driver
      class(Logger), pointer :: lgr
      character(:), allocatable :: this_name
      type(HierarchyResources), pointer :: resources

      _ASSERT(this%children%count(child_name) == 0, 'duplicate child name: <'//child_name//'>.')
      ! Hierarchy-wide name uniqueness. Sibling check above
      ! is not sufficient; names must be unique across the entire hierarchy.
      resources => get_hierarchy_resources(this%hierarchy_id)
      _ASSERT(resources%component_names%count(child_name) == 0, 'duplicate component name elsewhere in the hierarchy: <'//child_name//'>.')

      call create_child(child_name, child_spec, this%hconfig, this%registry, &
           this%user_offset, this%hierarchy_id, child_driver, _RC)

      call this%children%insert(child_name, child_driver)
      ! Record this name in the hierarchy-wide registry (graph/
      ! component-hierarchy delta spec) only once the child is fully
      ! and successfully added - matching the sibling-level
      ! this%children%insert() above, not inserted speculatively before
      ! construction could still fail.
      call resources%component_names%insert(child_name)

      lgr => this%get_logger()
      this_name = this%get_name() ! workaround for gfortran
      call lgr%debug('%a added child <%a~>', this_name, child_name, _RC)

      _RETURN(_SUCCESS)

   end subroutine add_child_by_spec

end submodule add_child_by_spec_smod
