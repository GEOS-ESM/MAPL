#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) propagate_geom_to_children_smod
   use mapl_GeometrySpec_mod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module subroutine propagate_geom_to_children(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call apply_to_children(this, set_child_geom, _RC)

      _RETURN(ESMF_SUCCESS)
   contains

      subroutine set_child_geom(this, child_meta, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) :: child_meta
         integer, optional, intent(out) :: rc

         associate(kind => child_meta%component_spec%geometry_spec%kind)
            _RETURN_IF(kind /= GEOMETRY_FROM_PARENT)

            child_meta%geom_id = this%geom_id
            if (allocated(this%geom)) then
               call child_meta%set_geom(this%geom)
            end if
            if (allocated(this%vertical_grid)) then
               call child_meta%set_vertical_grid(this%vertical_grid)
            end if
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine set_child_geom

   end subroutine propagate_geom_to_children

end submodule propagate_geom_to_children_smod
