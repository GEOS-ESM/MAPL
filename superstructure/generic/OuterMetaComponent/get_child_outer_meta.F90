#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_child_outer_meta_smod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   ! Same get_child() -> get_gridcomp() -> get_outer_meta() reach as
   ! get_child_component_spec()/get_child_component_graph() above - see
   ! the module-level interface comment for the visibility rationale.
   module function get_child_outer_meta(this, child_name, rc) result(child_meta)
      type(OuterMetaComponent), pointer :: child_meta
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child_driver
      type(ESMF_GridComp) :: child_gc

      child_meta => null()
      child_driver = this%get_child(child_name, _RC)
      child_gc = child_driver%get_gridcomp()
      child_meta => get_outer_meta(child_gc, _RC)

      _RETURN(_SUCCESS)
   end function get_child_outer_meta

end submodule get_child_outer_meta_smod
