#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_child_component_spec_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   ! Same get_child() -> get_gridcomp() -> get_outer_meta() reach
   ! Test_ComponentHierarchyGraph.pf already uses for test setup, and
   ! propagate_geom_to_children.F90/apply_to_children_custom.F90 use for
   ! the framework's own child-mutation needs - see the module-level
   ! interface comment above for the visibility rationale.
   module function get_child_component_spec(this, child_name, rc) result(component_spec)
      type(ComponentSpec), pointer :: component_spec
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child_driver
      type(ESMF_GridComp) :: child_gc
      type(OuterMetaComponent), pointer :: child_meta

      component_spec => null()
      child_driver = this%get_child(child_name, _RC)
      child_gc = child_driver%get_gridcomp()
      child_meta => get_outer_meta(child_gc, _RC)
      component_spec => child_meta%get_component_spec()

      _RETURN(_SUCCESS)
   end function get_child_component_spec

end submodule get_child_component_spec_smod
