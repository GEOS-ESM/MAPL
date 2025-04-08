#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_geom_b_smod
   use mapl3g_GenericPhases
   use mapl3g_GeometrySpec
   use mapl_ErrorHandling
   implicit none(type,external)

contains
   
   ! In this sweep, components inherit geometry from their parent
   ! unless otherwise specified.
   module recursive subroutine initialize_geom_b(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_GEOM_B'
      
      call apply_to_children(this, set_child_geom, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_GEOM_B, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains
      
      subroutine set_child_geom(this, child_meta, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) ::  child_meta
         integer, optional, intent(out) :: rc
         
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

   end subroutine initialize_geom_b

end submodule initialize_geom_b_smod
