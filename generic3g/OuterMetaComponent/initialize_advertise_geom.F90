#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_advertise_geom_smod
   implicit none

contains

   ! ESMF initialize methods

   !----------
   !The parent geom can be overridden by a
   ! component by:
   !   - providing a geom spec in the generic section of its config
   !     file, or
   !   - specifying an INIT_GEOM phase
   ! If both are specified, the INIT_GEOM overrides the config spec.
   !----------
   module recursive subroutine initialize_advertise_geom(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom), pointer :: mapl_geom
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE_GEOM'
      type(GeomManager), pointer :: geom_mgr
      class(GriddedComponentDriver), pointer :: provider
      type(ESMF_GridComp) :: provider_gc
      type(OuterMetaComponent), pointer :: provider_meta

      associate (geometry_spec => this%component_spec%geometry_spec)
        if (allocated(geometry_spec%geom_spec)) then
           geom_mgr => get_geom_manager()
           mapl_geom => geom_mgr%get_mapl_geom(geometry_spec%geom_spec, _RC)
           this%geom = mapl_geom%get_geom()
        end if
        if (allocated(geometry_spec%vertical_grid)) then
           this%vertical_grid = geometry_spec%vertical_grid
        end if

        call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)

        call recurse(this, phase_idx=GENERIC_INIT_ADVERTISE_GEOM, _RC)

        if (geometry_spec%kind == GEOMETRY_FROM_CHILD) then
           provider => this%children%at(geometry_spec%provider, _RC)
           provider_gc = provider%get_gridcomp()
           provider_meta => get_outer_meta(provider_gc, _RC)
           _ASSERT(allocated(provider_meta%geom), 'Specified child does not provide a geom.')
           this%geom = provider_meta%geom
        end if
      end associate

      _RETURN(ESMF_SUCCESS)
   contains

   end subroutine initialize_advertise_geom

end submodule initialize_advertise_geom_smod
