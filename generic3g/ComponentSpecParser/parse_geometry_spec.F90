#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_geometry_spec_smod

   use mapl3g_VerticalGrid
   use mapl3g_BasicVerticalGrid
   use mapl3g_VerticalGrid_API
   use mapl3g_ModelVerticalGrid

   implicit none(external,type)

contains

   ! Geom subcfg is passed raw to the GeomManager layer.  So little
   ! processing is needed here.
   module function parse_geometry_spec(mapl_cfg, registry, rc) result(geometry_spec)
      type(GeometrySpec) :: geometry_spec
      type(ESMF_HConfig), intent(in) :: mapl_cfg
      type(StateRegistry), target, intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_geometry_section
      logical :: has_esmf_geom
      logical :: has_vertical_grid
      logical :: has_geometry_kind
      logical :: has_geometry_provider
      character(:), allocatable :: geometry_kind_str
      character(:), allocatable :: provider
      type(ESMF_HConfig) :: geometry_cfg
      type(ESMF_HConfig) :: esmf_geom_cfg
      type(ESMF_HConfig) :: vertical_grid_cfg
      type(GeomManager), pointer :: geom_mgr
      class(GeomSpec), allocatable :: geom_spec
      class(VerticalGrid), allocatable :: vertical_grid

      has_geometry_section = ESMF_HConfigIsDefined(mapl_cfg,keyString=COMPONENT_GEOMETRY_SECTION, _RC)
      _RETURN_UNLESS(has_geometry_section)

      geometry_cfg = ESMF_HConfigCreateAt(mapl_cfg, keyString=COMPONENT_GEOMETRY_SECTION, _RC)

      has_geometry_kind = ESMF_HConfigIsDefined(geometry_cfg, keyString='kind', _RC)
      has_esmf_geom = ESMF_HConfigIsDefined(geometry_cfg, keyString=COMPONENT_ESMF_GEOM_SECTION, _RC)
      has_vertical_grid = ESMF_HConfigIsDefined(geometry_cfg, keyString=COMPONENT_VERTICAL_GRID_SECTION, _RC)

      if (.not. (has_geometry_kind .or. has_esmf_geom .or. has_vertical_grid)) then ! default
         geometry_spec = GeometrySpec(GEOMETRY_FROM_PARENT)
         call ESMF_HConfigDestroy(geometry_cfg, _RC)
         _RETURN(_SUCCESS)
      end if

      if (has_geometry_kind) then
         geometry_kind_str = ESMF_HConfigAsString(geometry_cfg, keyString='kind', _RC)
      end if

      if (has_esmf_geom) then
         esmf_geom_cfg = ESMF_HConfigCreateAt(geometry_cfg, keyString=COMPONENT_ESMF_GEOM_SECTION, _RC)
      end if

      if (has_vertical_grid) then
         vertical_grid_cfg = ESMF_HConfigCreateAt(geometry_cfg, keyString=COMPONENT_VERTICAL_GRID_SECTION, _RC)
      end if

      if (has_geometry_kind .and. (has_esmf_geom .or. has_vertical_grid)) then
         _ASSERT(geometry_kind_str == 'provider', 'Geometry kind must be provider when using ESMF geom config or vertical grid.')
      end if

      if (.not. (has_esmf_geom .or. has_vertical_grid)) then ! must have provided kind
         select case (ESMF_UtilStringLowerCase(geometry_kind_str))
         case ('none')
            geometry_spec = GeometrySpec(GEOMETRY_NONE)
         case ('provider')
            geometry_spec = GeometrySpec(GEOMETRY_PROVIDER)
         case ('from_parent')
            geometry_spec = GeometrySpec(GEOMETRY_FROM_PARENT)
         case ('from_child')
            has_geometry_provider = ESMF_HConfigIsDefined(geometry_cfg, keystring='provider', _RC)
            _ASSERT(has_geometry_provider, 'Must name provider when using GEOMETRY_FROM_CHILD')
            provider = ESMF_HConfigAsString(geometry_cfg, keystring='provider', _RC)
            geometry_spec = GeometrySpec(provider)
         case default
            _FAIL('Invalid geometry kind')
         end select
         call ESMF_HConfigDestroy(geometry_cfg, _RC)
         _RETURN(_SUCCESS)
      end if

      if (has_esmf_geom) then
         geom_mgr => get_geom_manager()
         allocate(geom_spec, source=geom_mgr%make_geom_spec(esmf_geom_cfg, rc=status))
         _VERIFY(status)
         call ESMF_HConfigDestroy(geometry_cfg, _RC)
      end if

      if (has_vertical_grid) then
         call parse_vertical_grid_(vertical_grid_cfg, registry, vertical_grid, _RC)
      end if
      geometry_spec = GeometrySpec(geom_spec=geom_spec, vertical_grid=vertical_grid)

      _RETURN(_SUCCESS)
   end function parse_geometry_spec

   subroutine parse_vertical_grid_(vertical_grid_cfg, registry, vertical_grid, rc)
      type(ESMF_HConfig), intent(in) :: vertical_grid_cfg
      type(StateRegistry), target, intent(in) :: registry
      class(VerticalGrid), allocatable, intent(out) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status
      type(VerticalGridManager), pointer :: vgrid_manager
      class(VerticalGrid), pointer :: vgrid

      vgrid_manager => get_vertical_grid_manager(_RC)
      vgrid => vgrid_manager%create_grid(vertical_grid_cfg, _RC)

      ! ModelVerticalGrid needs a registry which cannot be derived from a config.
      ! This should only be used in testing.
      select type(vgrid)
      type is(ModelVerticalGrid)
         call vgrid%set_registry(registry)
      end select

      vertical_grid = vgrid

      _RETURN(_SUCCESS)
   end subroutine parse_vertical_grid_

end submodule parse_geometry_spec_smod
