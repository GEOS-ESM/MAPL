#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) initialize_geom_a_smod
   use mapl3g_GenericPhases
   use mapl3g_GeometrySpec
   use mapl3g_Geom_API
   use mapl3g_GriddedComponentDriver
   use mapl_ErrorHandling
   implicit none(type,external)

contains
   
   ! In this sweep, components can specify their own geometry or use
   ! the geometry of a designated child.
   module recursive subroutine initialize_geom_a(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_GEOM_A'
      class(GriddedComponentDriver), pointer :: provider
      type(ESMF_GridComp) :: provider_gc
      type(OuterMetaComponent), pointer :: provider_meta
      type(MaplGeom), pointer :: mapl_geom
      type(GeomManager), pointer :: geom_mgr

      ! Handle case where component provides its own geometry.
       associate (geometry_spec => this%component_spec%geometry_spec)
        if (allocated(geometry_spec%geom_spec)) then
           geom_mgr => get_geom_manager()
           mapl_geom => geom_mgr%get_mapl_geom(geometry_spec%geom_spec, _RC)
           this%geom = mapl_geom%get_geom()
        end if
        if (allocated(geometry_spec%vertical_grid)) then
           this%vertical_grid = geometry_spec%vertical_grid
        end if
      end associate

      call recurse(this, phase_idx=GENERIC_INIT_GEOM_A, _RC)

      associate (geometry_spec => this%component_spec%geometry_spec)
        if (geometry_spec%kind == GEOMETRY_FROM_CHILD) then
           provider => this%children%at(geometry_spec%provider, _RC)
           provider_gc = provider%get_gridcomp()
           provider_meta => get_outer_meta(provider_gc, _RC)
           _ASSERT(allocated(provider_meta%geom), 'Specified child does not provide a geom.')
           this%geom = provider_meta%geom
           this%vertical_grid = provider_meta%vertical_grid
        end if
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_geom_a

end submodule initialize_geom_a_smod
