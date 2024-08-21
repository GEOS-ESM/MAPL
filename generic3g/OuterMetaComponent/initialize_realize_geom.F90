#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_realize_geom_smod
   implicit none

contains

   !----------
   ! The procedure initialize_realize_geom() is responsible for passing grid
   ! down to children.
   ! ---------
   module recursive subroutine initialize_realize_geom(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom), pointer :: mapl_geom
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_REALIZE_GEOM'
      type(GeomManager), pointer :: geom_mgr

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call apply_to_children(this, set_child_geom, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_REALIZE_GEOM, _RC)

      _RETURN(ESMF_SUCCESS)
   contains

      subroutine set_child_geom(this, child_meta, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) ::  child_meta
         integer, optional, intent(out) :: rc

         integer :: status

         associate(kind => child_meta%component_spec%geometry_spec%kind)
           _RETURN_IF(kind /= GEOMETRY_FROM_PARENT)

           if (allocated(this%geom)) then
              call child_meta%set_geom(this%geom)
           end if
           if (allocated(this%vertical_grid)) then
              call child_meta%set_vertical_grid(this%vertical_grid)
           end if
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine set_child_geom

   end subroutine initialize_realize_geom

end submodule initialize_realize_geom_smod
