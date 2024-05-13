#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_geometry_spec_smod
   
contains

   ! Geom subcfg is passed raw to the GeomManager layer.  So little
   ! processing is needed here.
   module function parse_geometry_spec(mapl_cfg, rc) result(geometry_spec)
      type(GeometrySpec) :: geometry_spec
      type(ESMF_HConfig), intent(in) :: mapl_cfg
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_geometry_section
      logical :: has_esmf_geom
      logical :: has_geometry_kind
      logical :: has_geometry_provider
      character(:), allocatable :: geometry_kind_str
      character(:), allocatable :: provider
      integer :: geometry_kind
      type(ESMF_HConfig) :: geometry_cfg
      type(ESMF_HConfig) :: esmf_geom_cfg
      type(GeomManager), pointer :: geom_mgr
      class(GeomSpec), allocatable :: geom_spec

      has_geometry_section = ESMF_HConfigIsDefined(mapl_cfg,keyString=COMPONENT_GEOMETRY_SECTION, _RC)
      _RETURN_UNLESS(has_geometry_section)

      geometry_cfg = ESMF_HConfigCreateAt(mapl_cfg, keyString=COMPONENT_GEOMETRY_SECTION, _RC)

      has_geometry_kind = ESMF_HConfigIsDefined(geometry_cfg, keyString='kind', _RC)
      has_esmf_geom = ESMF_HConfigIsDefined(geometry_cfg, keyString=COMPONENT_ESMF_GEOM_SECTION, _RC)

      if (.not. (has_geometry_kind .or. has_esmf_geom)) then ! default
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

      if (has_geometry_kind .and. has_esmf_geom) then
         _ASSERT(geometry_kind_str == 'provider', 'Geometry kind must be provider when using ESMF geom config.')
      end if

      if (has_esmf_geom) then
         geom_mgr => get_geom_manager()
         allocate(geom_spec, source=geom_mgr%make_geom_spec(esmf_geom_cfg, rc=status))
         _VERIFY(status)
         call ESMF_HConfigDestroy(geometry_cfg, _RC)
         geometry_spec = GeometrySpec(geom_spec)
         _RETURN(_SUCCESS)
      end if

      if (has_geometry_kind) then
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
      end if

      _RETURN(_SUCCESS)
   end function parse_geometry_spec

end submodule parse_geometry_spec_smod

