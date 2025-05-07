#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_geometry_spec_smod

   use mapl3g_VerticalGrid
   use mapl3g_BasicVerticalGrid
   use mapl3g_FixedLevelsVerticalGrid
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

      integer :: num_levels
      character(:), allocatable :: class, standard_name, units
      real, allocatable :: levels(:)
      integer :: status

      class = ESMF_HConfigAsString(vertical_grid_cfg, keyString="class", _RC)
      select case(class)
      case("basic")
         num_levels = ESMF_HConfigAsI4(vertical_grid_cfg, keyString="num_levels", _RC)
         vertical_grid = BasicVerticalGrid(num_levels)
      case("fixed_levels")
         standard_name = ESMF_HConfigAsString(vertical_grid_cfg, keyString="standard_name", _RC)
         units = ESMF_HConfigAsString(vertical_grid_cfg, keyString="units", _RC)
         levels = ESMF_HConfigAsR4Seq(vertical_grid_cfg, keyString="levels" ,_RC)
         vertical_grid = FixedLevelsVerticalGrid(standard_name, levels, units)
      case("model")
         call parse_model_vertical_grid_(vertical_grid_cfg, registry, vertical_grid, _RC)
      case default
         _FAIL("vertical grid class "//class//" not supported")
      end select

      _RETURN(_SUCCESS)
   end subroutine parse_vertical_grid_

   subroutine parse_model_vertical_grid_(vertical_grid_cfg, registry, vertical_grid, rc)
      type(ESMF_HConfig), intent(in) :: vertical_grid_cfg
      type(StateRegistry), target, intent(in) :: registry
      class(VerticalGrid), allocatable, intent(out) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: num_levels
      character(:), allocatable :: standard_name, units, field_edge, field_center
      integer :: status

      standard_name = ESMF_HConfigAsString(vertical_grid_cfg, keyString="standard_name", _RC)
      units = ESMF_HConfigAsString(vertical_grid_cfg, keyString="units", _RC)
      num_levels = ESMF_HConfigAsI4(vertical_grid_cfg, keyString="num_levels", _RC)
      vertical_grid = ModelVerticalGrid(standard_name=standard_name, units=units, num_levels=num_levels)
      field_edge = ESMF_HConfigAsString(vertical_grid_cfg, keyString="field_edge", _RC)
      field_center = ESMF_HConfigAsString(vertical_grid_cfg, keyString="field_center", _RC)
      select type(vertical_grid)
      type is(ModelVerticalGrid)
         call vertical_grid%set_registry(registry)
         call vertical_grid%add_short_name(edge=field_edge, center=field_center)
      end select

      _RETURN(_SUCCESS)
   end subroutine parse_model_vertical_grid_

end submodule parse_geometry_spec_smod
